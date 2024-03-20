pragma Ada_2012;

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

with AWS.Client;
with AWS.Messages;
with AWS.Response;
with AWS.Translator;

with EVIL.Util.Files;
with Export_Utils;
with Load_Environment_Variables;
with Util.Log.Loggers;

package body Export_Manager.Neo4J is
   use AWS.Client;
   use AWS.Response;

   use type Ada.Containers.Count_Type;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Export_Manager.Neo4J");

   NEO4J_WORKING_DIRECTORY : constant String := (if Ada.Environment_Variables.Exists ("NEO4J_RESULT_DIR")
   then Ada.Environment_Variables.Value ("NEO4J_RESULT_DIR")
   else Ada.Directories.Compose (Ada.Directories.Current_Directory, ".atgdb"));

   --------------------------------------
   -- Get_Boolean_Environment_Variable --
   --------------------------------------

   function Get_Boolean_Environment_Variable (Name : String; Default_Value : Boolean) return Boolean is
   begin
      if not Ada.Environment_Variables.Exists (Name) then
         return Default_Value;
      end if;

      begin
         return Boolean'Value (Ada.Environment_Variables.Value (Name));
      exception
         when Constraint_Error =>
            return Default_Value;
      end;
   end Get_Boolean_Environment_Variable;

   DRY_RUN : constant Boolean := Get_Boolean_Environment_Variable (Name => "DRY_RUN", Default_Value => False);
   MAX_RECORDS_UPLOAD_LIMIT : constant         := 1_000;
   MIN_RECORDS_UPLOAD_LIMIT : constant         := 100;
   Connection               : HTTP_Connection_Access;
   MAX_ELEMENTS_PER_FILE    : constant         := 100_000;

   package JSON_Value_Vector is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => JSON_Value);
   use JSON_Value_Vector;

   protected Queue is
      entry Add (Element : JSON_Value);
      entry Get_Elements (Elements : out Vector);
      function Length return Natural;
      procedure Set_Exit_Process (Value : Boolean := True);
   private
      List         : Vector  := JSON_Value_Vector.Empty_Vector;
      Exit_Process : Boolean := False;
   end Queue;

   protected body Queue is
      entry Add (Element : JSON_Value) when List.Length < MAX_RECORDS_UPLOAD_LIMIT is
      begin
         List.Append (Element);
      end Add;

      entry Get_Elements (Elements : out Vector) when List.Length >= MIN_RECORDS_UPLOAD_LIMIT or Exit_Process is
      begin
         Elements := List;
         List.Clear;
      end Get_Elements;

      function Length return Natural is
      begin
         return Natural (List.Length);
      end Length;

      procedure Set_Exit_Process (Value : Boolean := True) is
      begin
         Exit_Process := Value;
      end Set_Exit_Process;
   end Queue;

   --------------
   -- Watchdog --
   --------------

   task Watchdog is
      entry Initialize;
      entry Alive;
      entry Finalize;
   end Watchdog;

   --------------------
   -- Upload_Manager --
   --------------------

   task Upload_Manager is
      entry Start;
   end Upload_Manager;

   task body Watchdog is
   begin
      select
         accept Initialize;
      or
         terminate;
      end select;

      Upload_Manager.Start;

      loop
         select
            accept Finalize;
            exit;
         or
            accept Alive;
         or
            terminate;
         end select;
      end loop;
   end Watchdog;

   task body Upload_Manager is
      use Ada.Streams, Ada.Text_IO, Ada.Text_IO.Text_Streams;

      procedure Custom_Open_File
        (File : in out File_Type; Mode : File_Mode; Name : String; Form : String := "wcem=8")
      is
      begin
         Open (File, Mode, Name, Form);
      end Custom_Open_File;

      procedure Custom_Create_File
        (File : in out File_Type; Mode : File_Mode; Name : String; Form : String := "wcem=8")
      is
      begin
         Create (File, Mode, Name, Form);
      end Custom_Create_File;

      package Evil_Files is new EVIL.Util.Files
        (Character     => Character, String => String, File_Type => File_Type, File_Mode => File_Mode,
         Stream_Access => Stream_Access, Open => Custom_Open_File, Create => Custom_Create_File);
      use Evil_Files;

      type File_Access is access Evil_Files.File;

      type File_Info is record
         File        : File_Access;
         Nb_Elements : Natural;
         Chunk       : Positive;
      end record;

      package File_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String, Element_Type => File_Info, Hash => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => "=", "=" => "=");
      use File_Maps;

      package Node_Id_Sets is new Ada.Containers.Hashed_Sets
        (Element_Type => Unbounded_String, Hash => Ada.Strings.Unbounded.Hash, Equivalent_Elements => "=",
         "="          => "=");
      use Node_Id_Sets;

      procedure Open_File
        (File : in out File_Type; Mode : File_Mode; Path : String; File_Form : String := "wcem=8";
         Auto :        Boolean := True)
      is
         use Ada.Directories;
      begin
         if Exists (Path) then
            Open (File, Mode, Path, File_Form);
         else
            if Auto then
               Create (File, Mode, Path, File_Form);
            else
               raise Ada.Directories.Name_Error;
            end if;
         end if;
      end Open_File;

      function Open_File (Path : String; Mode : File_Mode; Auto : Boolean := True) return Evil_Files.File is
         use Ada.Directories;
      begin
         if Exists (Path) then
            return Open (Path, Mode);
         else
            if Auto then
               return Create (Path, Mode);
            else
               raise Ada.Directories.Name_Error;
            end if;
         end if;
      end Open_File;

      procedure Close_Map_File (The_Map : Map; Key : Unbounded_String) is
      begin
         String'Write (The_Map.Element (Key).File.Stream, "]}}]}");
         The_Map.Element (Key).File.Close;
      end Close_Map_File;

      procedure Add_To_Map
        (The_Map           : in out Map; Key : Unbounded_String; Value : JSON_Value; Prefix_Filename : String;
         Init_File_Content :        String)
      is
         use Ada.Strings;
         use Ada.Strings.Fixed;
         Map_Element : File_Info;
         Separator   : Unbounded_String := To_Unbounded_String (",");
      begin
         if The_Map.Contains (Key) then
            Map_Element := The_Map.Element (Key);

            if Map_Element.Nb_Elements = MAX_ELEMENTS_PER_FILE then
               -- Create another chunck
               Close_Map_File (The_Map, Key);
               Map_Element.Chunk       := Map_Element.Chunk + 1;
               Map_Element.Nb_Elements := 1;
               Map_Element.File        :=
                 new Evil_Files.File'
                   (Open_File
                      (Ada.Directories.Compose
                         (NEO4J_WORKING_DIRECTORY,
                          Prefix_Filename & To_String (Key) & "_" & Trim (Positive'Image (Map_Element.Chunk), Both) &
                          ".json"),
                       Append_File));
               Separator               := Null_Unbounded_String;
               String'Write (Map_Element.File.Stream, Init_File_Content);
            else
               Map_Element.Nb_Elements := Map_Element.Nb_Elements + 1;
            end if;
         else
            -- Create the file
            Map_Element :=
              (File        =>
                 new Evil_Files.File'
                   (Open_File
                      (Ada.Directories.Compose
                         (NEO4J_WORKING_DIRECTORY, Prefix_Filename & To_String (Key) & "_1.json"),
                       Append_File)),
               Nb_Elements => Positive'First, Chunk => Positive'First);
            Separator   := Null_Unbounded_String;
            String'Write (Map_Element.File.Stream, Init_File_Content);
         end if;
         String'Write (Map_Element.File.Stream, To_String (Separator) & Value.Write);
         The_Map.Include (Key, Map_Element);
      end Add_To_Map;

      procedure Send_Query (Query : Stream_Element_Array) is
         use AWS.Messages;

         Response : AWS.Response.Data;
      begin
         if DRY_RUN then
            return;
         end if;

         -- Sends the request to the server
         Post (Connection.all, Response, Query, "application/json");

         -- Response code should be: 200
         case AWS.Response.Status_Code (Response) is
            when Success =>
               -- It returns a string that represent a JSON_Object that should contains:
               -- - results : JSON_Array
               -- - errors : JSON_Array
               --
               -- Errors are JSON_Object that contains:
               -- - code : String
               -- - message : String
               declare
                  Body_String   : constant String     := Message_Body (Response);
                  Response_JSON : constant JSON_Value := Read (Body_String);
                  Errors_Array  : constant JSON_Array := Response_JSON.Get ("errors");
                  Errors        : Unbounded_String    := Null_Unbounded_String;

               begin
                  if not GNATCOLL.JSON.Is_Empty (Errors_Array) then
                     for Index in Positive'First .. GNATCOLL.JSON.Length (Response_JSON.Get ("errors")) loop
                        declare
                           code    : constant String := Get (Errors_Array, Index).Get ("code");
                           message : constant String := Get (Errors_Array, Index).Get ("message");
                        begin
                           Errors :=
                             Errors & To_Unbounded_String (code) & ": " & To_Unbounded_String (message) & ASCII.LF;
                        end;
                     end loop;
                     Log.Error ("End response errors: {0}" & ASCII.LF, To_String (Errors));
                  end if;
               end;
            when Client_Error | Server_Error =>
               Log.Error ("Start Client_Error | Server_Error");
               Log.Error
                 ("Upload_Manager error in HTTP response: " & Reason_Phrase (AWS.Response.Status_Code (Response)));
               raise Program_Error with Reason_Phrase (AWS.Response.Status_Code (Response));
            when others =>
               Log.Info ("Other response code");
               -- Other HTTP response code are not important
               null;
         end case;
      end Send_Query;

      procedure Send_Query (Query : String) is
         use AWS.Translator;

         -- The use of variable for the conversion to String is to prevent stackoverflow due to
         -- heap size and conversion implementation depending on the OS and the CPU architecture.
         --
         -- The none use of variable raise:
         -- - CONSTRAINT_ERROR : erroneous memory access
         --
         -- When the string request is huge.
         Queries_Stream : constant Stream_Element_Array := To_Stream_Element_Array (Query);
      begin
         Send_Query (Queries_Stream);
      end Send_Query;

      function Concat_Labels (Labels : JSON_Array; Separator : Character) return Unbounded_String is
         S      : constant String  := GNATCOLL.JSON.Get (Get (Labels, 1));
         Result : Unbounded_String := To_Unbounded_String (S);
      begin
         for I in 2 .. Length (Labels) loop
            declare
               S : constant String := Get (Get (Labels, I));
            begin
               Result := Result & Separator & To_Unbounded_String (S);
            end;
         end loop;
         return Result;
      end Concat_Labels;

      Node_Id_Set           : Set := Empty_Set;
      Node_File_Map         : Map := Empty_Map;
      Relationship_File_Map : Map := Empty_Map;

      procedure Process_Node (Element : JSON_Value) is
         use Export_Utils;

         Node_Id    : constant String     := Element.Get ("node_id");
         Properties : constant JSON_Value := Create_Object;
      begin
         if Node_Id_Set.Contains (To_Unbounded_String (Node_Id)) then
            return;
         end if;

         Node_Id_Set.Include (To_Unbounded_String (Node_Id));

         Get_Node_Properties :
         declare
            procedure Add_Property (Key : String) is
            begin
               if not Element.Has_Field (Key) then
                  return;
               end if;

               case Element.Get (Key).Kind is
                  when JSON_Boolean_Type =>
                     declare
                        Value : constant Boolean := Element.Get (Key);
                     begin
                        Properties.Set_Field (Key, Value);
                     end;
                  when JSON_Int_Type =>
                     declare
                        Value : constant Integer := Element.Get (Key);
                     begin
                        Properties.Set_Field (Key, Value);
                     end;
                  when JSON_String_Type =>
                     declare
                        Value : constant String := Element.Get (Key);
                     begin
                        Properties.Set_Field (Key, Value);
                     end;
                  when others =>
                     Log.Error
                       ("Error, JSON_Value_Type not supported: " & JSON_Value_Type'Image (Element.Get (Key).Kind));
               end case;
            end Add_Property;

            Filename : constant String  := Element.Get ("location").Get ("filename");
            Line     : constant Integer := Element.Get ("location").Get ("line");
            Column   : constant Integer := Element.Get ("location").Get ("column");
            Kinds    : JSON_Array;
         begin -- Get_Node_Properties
            if Element.Has_Field ("kinds") then
               Kinds := Element.Get ("kinds");
            end if;

            if Is_Empty (Kinds) then
               if not Element.Has_Field ("kind") then
                  Log.Error ("No 'kind' nor 'kinds' found for node: " & Element.Write);
                  raise Program_Error with "No 'kind' nor 'kinds' found for node: " & Element.Write;
               end if;
               Append (Kinds, Element.Get ("kind"));
            end if;

            Properties.Set_Field ("node_id", Node_Id);
            Properties.Set_Field ("kinds", Kinds);
            -- Location
            Properties.Set_Field ("filename", Filename);
            Properties.Set_Field ("line", Line);
            Properties.Set_Field ("column", Column);

            -- Element properties
            Add_Property ("content");
            Add_Property ("element_kind");
            Add_Property ("enclosing_unit");
            Add_Property ("is_part_of_implicit");
            Add_Property ("is_part_of_inherited");
            Add_Property ("is_part_of_instance");
            Add_Property ("special_case");
            Add_Property ("is_default_parameter");
            Add_Property ("is_named_parameter_association");

            -- Compilation unit properties
            Add_Property ("is_predefined_unit");
         end Get_Node_Properties;

         Append_In_File :
         declare
            Basename : constant Unbounded_String := Concat_Labels (Properties.Get ("kinds"), '+');

            Current_Node : constant JSON_Value := Create_Object;

            Data : constant String :=
              "{""statements"": [{" & """statement"": ""UNWIND $rows AS row\n" &
              "CREATE(n: `UNIQUE IMPORT LABEL`{node_id: row._id}) SET n += row.properties SET n:" &
              To_String (Concat_Labels (Properties.Get ("kinds"), ':')) & ";"" ," & ASCII.LF &
              """parameters"": {""rows"":[";
         begin
            Current_Node.Set_Field ("_id", Node_Id);
            Current_Node.Set_Field ("properties", Properties);

            Add_To_Map (Node_File_Map, Basename, Current_Node, "1_", Data);
         end Append_In_File;

         -- Create relationships

         for Kind in Relation_Kinds loop
            if not Element.Has_Field (Relation_Kinds'Image (Kind)) then
               goto Continue_Append_Relationship;
            end if;
            Append_Relationship :
            declare
               Basename : constant Unbounded_String := To_Unbounded_String (Relation_Kinds'Image (Kind));

               Current_Node   : constant JSON_Value := Create_Object;
               Start_Node     : constant JSON_Value := Create_Object;
               End_Node       : constant JSON_Value := Create_Object;
               Relation_Props : constant JSON_Value := Create_Object;
               End_Node_Id    : constant String     := Element.Get (To_String (Basename)).Get ("node_id");

               Data : constant String :=
                 "{""statements"": [{" & """statement"": ""UNWIND $rows AS row\n" &
                 "MATCH (start:`UNIQUE IMPORT LABEL`{node_id: row.start._id})\n" &
                 "MATCH (end:`UNIQUE IMPORT LABEL`{node_id: row.end._id})\n" & "CREATE (start)-[r:" &
                 To_String (Basename) & "]->(end) SET r += row.properties;"" ," & ASCII.LF &
                 """parameters"": {""rows"":[";
            begin
               Start_Node.Set_Field ("_id", Node_Id);
               End_Node.Set_Field ("_id", End_Node_Id);
               Current_Node.Set_Field ("start", Start_Node);
               Current_Node.Set_Field ("end", End_Node);
               Current_Node.Set_Field ("properties", Relation_Props);

               Add_To_Map (Relationship_File_Map, Basename, Current_Node, "2_", Data);
            end Append_Relationship;
            <<Continue_Append_Relationship>>
         end loop;
      end Process_Node;

      Init_File_Path      : constant String := Ada.Directories.Compose (NEO4J_WORKING_DIRECTORY, "0_init.json");
      Fin_File_Path       : constant String := Ada.Directories.Compose (NEO4J_WORKING_DIRECTORY, "3_fin_");
      Init_File, Fin_File : File_Type;
      Exit_Loop           : Boolean         := False;
   begin -- Upload_Manager
      select
         accept Start;
      or
         terminate;
      end select;
      Log.Debug ("[Upload_Manager] Start");

      if Ada.Directories.Exists (NEO4J_WORKING_DIRECTORY) then
         Ada.Directories.Delete_Tree (NEO4J_WORKING_DIRECTORY);
      end if;

      Ada.Directories.Create_Path (NEO4J_WORKING_DIRECTORY);

      if not DRY_RUN then
         Wait_HTTP_Connection_Ready :
         declare
            --  This type allows to show a Debug message every second, instead of every 100ms
            type Sec is mod 10;

            Exit_Loop : Boolean := False;
            Counter   : Sec     := Sec'First;
         begin
            loop
               if Connection = null then
                  if Counter = Sec'First then
                     Log.Debug ("Wait for connection ready");
                  end if;
                  delay 0.1;
                  Counter := Counter + 1;
               else
                  Exit_Loop := True;
               end if;

               exit when Exit_Loop;
            end loop;
         end Wait_HTTP_Connection_Ready;
      end if;

      Add_Indexes :
      declare
         Request           : constant JSON_Value                         := Create_Object;
         String_Statements : constant array (1 .. 3) of Unbounded_String :=
           (To_Unbounded_String
              ("CREATE CONSTRAINT uniqueNodeIds IF NOT EXISTS FOR (node:Element) REQUIRE (node.node_id) IS UNIQUE;"),
            To_Unbounded_String
              ("CREATE CONSTRAINT uniqueImportLabel IF NOT EXISTS FOR (node:`UNIQUE IMPORT LABEL`) REQUIRE" &
               " (node.node_id) IS UNIQUE;"),
            To_Unbounded_String ("CALL db.awaitIndexes(30);"));
         Statements        : JSON_Array;
      begin
         Open_File (Init_File, Append_File, Init_File_Path);
         for Statement_UStr of String_Statements loop
            declare
               Statement : constant JSON_Value := Create_Object;
            begin
               Statement.Set_Field ("statement", To_String (Statement_UStr));
               Append (Statements, Statement);
            end;
         end loop;

         Request.Set_Field ("statements", Statements);

         Put_Line (Init_File, Request.Write);

         Close (Init_File);

         Send_Query (Request.Write);
      end Add_Indexes;

      loop
         Get_Nodes :
         declare
            Elements : Vector;
         begin
            Queue.Get_Elements (Elements);

            for Elt : JSON_Value of Elements loop
               Process_Node (Elt);
            end loop;
         end Get_Nodes;

         if not Exit_Loop then
            begin
               Watchdog.Alive;
            exception
               when Tasking_Error =>
                  Exit_Loop := True;
            end;
         end if;
         exit when Exit_Loop and Queue.Length = 0;
      end loop;

      -- Free memory, because it is not used anymore
      Node_Id_Set := Empty_Set;

      for Cursor in Node_File_Map.Iterate loop
         Close_Map_File (Node_File_Map, Key (Cursor));
      end loop;

      for Cursor in Relationship_File_Map.Iterate loop
         Close_Map_File (Relationship_File_Map, Key (Cursor));
      end loop;

      Populate_Database :
      declare
         type Stream_Element_Array_Access is access Stream_Element_Array;

         SEA : Stream_Element_Array_Access;

         procedure Process_File (Cursor : File_Maps.Cursor; Prefix_Filename : String) is
         begin
            for Chunk_Id in 1 .. Element (Cursor).Chunk loop
               declare
                  use Ada.Directories;

                  File   : File_Access;
                  Path   : constant String                    :=
                    Ada.Directories.Compose
                      (NEO4J_WORKING_DIRECTORY,
                       Prefix_Filename & To_String (Key (Cursor)) & "_" &
                       Ada.Strings.Fixed.Trim (Positive'Image (Chunk_Id), Ada.Strings.Both) & ".json");
                  S      : constant Ada.Directories.File_Size := Size (Path);
                  Offset : Stream_Element_Offset;
               begin
                  SEA :=
                    new Stream_Element_Array'
                      (Stream_Element_Offset'First .. Stream_Element_Offset'First + Stream_Element_Offset (S) =>
                         Ada.Streams.Stream_Element (0));

                  File := new Evil_Files.File'(Open_File (Path, In_File, False));
                  Log.Debug ("Process: {0} ({1})", Path, File_Size'Image (S));
                  File.Stream.Read (SEA.all, Offset);
                  Send_Query (SEA.all);
                  File.Close;
               end;
            end loop;
         end Process_File;
      begin
         for Cursor in Node_File_Map.Iterate loop
            Process_File (Cursor, "1_");
         end loop;

         for Cursor in Relationship_File_Map.Iterate loop
            Process_File (Cursor, "2_");
         end loop;
      end Populate_Database;

      Add_Order_Of_Nodes :
      declare
         String_Statements : constant array (1 .. 6) of Unbounded_String :=
           (
         --  Create an order with relations to the parent
         To_Unbounded_String
              ("MATCH (p)<-[r:IS_ENCLOSED_IN]-(e:Element)" & ASCII.LF & "  WHERE r.index IS NULL" & ASCII.LF &
               "WITH p, e, r" & ASCII.LF & "  ORDER BY e.filename, e.line, e.column" & ASCII.LF & "CALL {" &
               ASCII.LF & "WITH p" & ASCII.LF & "MATCH (p)<-[r2:IS_ENCLOSED_IN]-(:Element)" & ASCII.LF &
               "  WHERE r2.index IS NOT NULL" & ASCII.LF & "RETURN count(r2) as nb" & ASCII.LF & "}" &
               ASCII.LF & "WITH p, nb, collect(distinct r) as rels" & ASCII.LF &
               "FOREACH (n IN rels | SET n.index = apoc.coll.indexOf(rels, n) + nb + 1);"),
         --  Create the call graph
            To_Unbounded_String
              ("MATCH (callerSpec)<-[:CORRESPONDING_SPECIFICATION]-(caller:" &
               "A_FUNCTION_BODY_DECLARATION|A_FUNCTION_BODY_STUB|A_PROCEDURE_BODY_DECLARATION|A_PROCEDURE_BODY_STUB|" &
               "A_PROTECTED_BODY_DECLARATION|A_PROTECTED_BODY_STUB|A_TASK_BODY_DECLARATION|A_TASK_BODY_STUB|" &
               "AN_ENTRY_BODY_DECLARATION|AN_EXPRESSION_FUNCTION_DECLARATION)<-[:IS_ENCLOSED_IN*]-" &
               "(subProgCall:A_PROCEDURE_CALL_STATEMENT|A_FUNCTION_CALL)<-[:IS_ENCLOSED_IN]-(id:AN_IDENTIFIER)-" &
               "[:CORRESPONDING_NAME_DEFINITION]->(:A_DEFINING_IDENTIFIER)-[:IS_ENCLOSED_IN]->()-" &
               "[:CORRESPONDING_SPECIFICATION]->(subProg)" & ASCII.LF &
               "MERGE (callerSpec)-[r:CALLING]->(subProg)" & ASCII.LF & "ON CREATE" & ASCII.LF &
               "  SET r.nbCall = 1" & ASCII.LF & "ON MATCH" & ASCII.LF & "  SET r.nbCall = r.nbCall + 1"),
         -- Create ancestors relationships
            To_Unbounded_String
              ("MATCH (e:A_DERIVED_RECORD_EXTENSION_DEFINITION|AN_INTERFACE_TYPE_DEFINITION|" &
               "A_DERIVED_TYPE_DEFINITION|A_FORMAL_DERIVED_TYPE_DEFINITION|A_FORMAL_INTERFACE_TYPE_DEFINITION|" &
               "A_PROTECTED_DEFINITION|A_TASK_DEFINITION)" & ASCII.LF & "MATCH (enclosingE)-" &
               "[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(e)" & ASCII.LF &
               "optional match (e)<-[:IS_ENCLOSED_IN]-()<-" &
               "[:IS_ENCLOSED_IN]-()-[:CORRESPONDING_NAME_DEFINITION]->()-[:IS_ENCLOSED_IN]->()-" &
               "[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(parent)" & ASCII.LF &
               "optional MATCH (enclosingParent)-" & "[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(parent)" &
               ASCII.LF & "CALL apoc.do.when(parent is not null, " &
               "'CALL apoc.create.relationship(enclosingParent,""IS_ANCESTOR_OF"",{},enclosingE) YIELD rel AS" &
               " relParent RETURN relParent, enclosingParent', 'return null', {enclosingE:enclosingE,enclosingParent:" &
               "enclosingParent}) yield value as ancestor" & ASCII.LF &
               "optional match (e)<-[:IS_ENCLOSED_IN]-" &
               "(:AN_IDENTIFIER)-[:CORRESPONDING_NAME_DEFINITION]->()-[:IS_ENCLOSED_IN]->()-[" &
               ":CORRESPONDING_TYPE_DECLARATION_VIEW]->(parentInterface)" & ASCII.LF & "optional MATCH " &
               "(enclosingParentInterface)-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(parentInterface)" &
               ASCII.LF & "call apoc.do.when(parentInterface is not null, 'CALL apoc.create.relationship(" &
               "enclosingParentInterface,""IS_PROGENITOR_OF"",{},enclosingE) YIELD rel AS relParentInterface RETURN " &
               "relParentInterface, enclosingParentInterface', 'return null', {enclosingE:enclosingE, " &
               "enclosingParentInterface:enclosingParentInterface}) yield value as interfaces" & ASCII.LF &
               "RETURN enclosingE, ancestor, interfaces"),
         -- Create ancestors relationships for tasks and protected
            To_Unbounded_String
              ("OPTIONAL MATCH (auie:A_TASK_TYPE_DECLARATION|A_SINGLE_TASK_DECLARATION|" &
               "A_PROTECTED_TYPE_DECLARATION|A_SINGLE_PROTECTED_DECLARATION)<-[:IS_ENCLOSED_IN*]-(:AN_IDENTIFIER)-" &
               "[:CORRESPONDING_NAME_DEFINITION]->()-[:IS_ENCLOSED_IN*]->(enclosingParentInterfaceTaskOrProtected" &
               ":A_DECLARATION)-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(parentInterfaceTaskOrProtected:" &
               "AN_INTERFACE_TYPE_DEFINITION)" & ASCII.LF &
               "call apoc.do.when(parentInterfaceTaskOrProtected is not " &
               "null, 'CALL apoc.create.relationship(enclosingParentInterfaceTaskOrProtected,""IS_PROGENITOR_OF"",{}," &
               "enclosingE) YIELD rel AS relParentInterfaceTaskOrProtected RETURN relParentInterfaceTaskOrProtected, " &
               "enclosingParentInterfaceTaskOrProtected', 'return null', {enclosingE:auie, " &
               "enclosingParentInterfaceTaskOrProtected:enclosingParentInterfaceTaskOrProtected}) yield value as " &
               "interfacesTaskOrProtected" & ASCII.LF & "RETURN *"),
         --  Remove label used in DB creation
            To_Unbounded_String ("MATCH (n:`UNIQUE IMPORT LABEL`) REMOVE n:`UNIQUE IMPORT LABEL`;"),
         --  Remove constraint used in DB creation
            To_Unbounded_String ("DROP CONSTRAINT uniqueImportLabel IF EXISTS;"));
      begin
         for Statement_Index in String_Statements'Range loop
            declare
               use Ada.Strings, Ada.Strings.Fixed;

               Request    : constant JSON_Value := Create_Object;
               Statements : JSON_Array;
               Statement  : constant JSON_Value := Create_Object;
            begin
               Open_File
                 (Fin_File, Append_File, Fin_File_Path & Trim (Positive'Image (Statement_Index), Both) & ".json");
               Statement.Set_Field ("statement", To_String (String_Statements (Statement_Index)));
               Append (Statements, Statement);
               Request.Set_Field ("statements", Statements);

               Put_Line (Fin_File, Request.Write);
               Close (Fin_File);

               Send_Query (Request.Write);
            end;
         end loop;
      end Add_Order_Of_Nodes;

      Log.Debug ("[Upload_Manager] End");
   exception
      when E : others =>
         Log.Debug ("[Upload_Manager] Exception: {0}", Ada.Exceptions.Exception_Name (E));
         Log.Debug ("[Upload_Manager] error: {0}", Ada.Exceptions.Exception_Information (E));
         Log.Error ("Upload_Manager error: ", E, Trace => True);
   end Upload_Manager;

   task body Instance is
      Exit_Loop : Boolean := False;
   begin

      while not Exit_Loop loop
         select
            accept Update (Element : JSON_Value) do
               Queue.Add (Element);
            end Update;
         or
            accept Finalize do
               Queue.Set_Exit_Process;
               Watchdog.Finalize;
               Exit_Loop := True;
            end Finalize;
         end select;
      end loop;
   end Instance;

   function Initialize return Observer_Type is
   begin
      Log.Debug ("DRY_RUN: {0}", Boolean'Image (DRY_RUN));

      Watchdog.Initialize;

      if not DRY_RUN then
         if Ada.Environment_Variables.Exists ("NEO4J_HOST") and Ada.Environment_Variables.Exists ("NEO4J_USER") and
           Ada.Environment_Variables.Exists ("NEO4J_PASS")
         then
            Connection :=
              new HTTP_Connection'
                (Create
                   (Host => Ada.Environment_Variables.Value ("NEO4J_HOST"),
                    User => Ada.Environment_Variables.Value ("NEO4J_USER"),
                    Pwd  => Ada.Environment_Variables.Value ("NEO4J_PASS")));
         else
            -- Missing a mandatory environment variables
            declare
               Error_Message : Unbounded_String := Null_Unbounded_String;
            begin
               if not Ada.Environment_Variables.Exists ("NEO4J_HOST") then
                  Append (Error_Message, "NEO4J_HOST");
               end if;

               if not Ada.Environment_Variables.Exists ("NEO4J_USER") then
                  if Length (Error_Message) > 0 then
                     Append (Error_Message, ", ");
                  end if;
                  Append (Error_Message, "NEO4J_USER");
               end if;

               if not Ada.Environment_Variables.Exists ("NEO4J_PASS") then
                  if Length (Error_Message) > 0 then
                     Append (Error_Message, ", ");
                  end if;
                  Append (Error_Message, "NEO4J_PASS");
               end if;

               raise Program_Error with "Missing " & To_String (Error_Message) & " environment variables.";
            end;
         end if;
      end if;

      return (Name => To_Unbounded_String ("Neo4J"), Job => new Instance);
   end Initialize;
end Export_Manager.Neo4J;
