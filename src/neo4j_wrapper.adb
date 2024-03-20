pragma Ada_2012;

with Ada.Containers.Vectors;
with Ada.Containers;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.Response;
with AWS.Translator;

with Load_Environment_Variables;
with Util.Log.Loggers;

package body neo4j_wrapper is
   use Ada.Strings.Unbounded;
   use AWS.Client;
   use AWS.Response;

   use type Ada.Containers.Count_Type;

   type Neo4j_Mode_Type is (HTTP_Neo4j_Mode, File_Neo4j_Mode);

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("neo4j_wrapper");

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

   -----------------------------------
   -- Get_Mode_Environment_Variable --
   -----------------------------------

   function Get_Mode_Environment_Variable (Name : String; Default_Value : Neo4j_Mode_Type) return Neo4j_Mode_Type is
   begin
      if not Ada.Environment_Variables.Exists (Name) then
         return Default_Value;
      end if;

      begin
         return Neo4j_Mode_Type'Value (Ada.Environment_Variables.Value (Name));
      exception
         when Constraint_Error =>
            return Default_Value;
      end;
   end Get_Mode_Environment_Variable;

   DEBUG                : constant Boolean := Get_Boolean_Environment_Variable (Name          => "NEO4J_DEBUG",
                                                                                Default_Value => False);
   DRY_RUN              : constant Boolean := Get_Boolean_Environment_Variable (Name          => "DRY_RUN",
                                                                                Default_Value => False);
   NEO4J_MODE           : constant Neo4j_Mode_Type := Get_Mode_Environment_Variable (Name          => "NEO4J_MODE",
                                                                                     Default_Value => File_Neo4j_Mode);
   MAX_RECORDS_UPLOAD_LIMIT : constant         := 1000;
   MIN_RECORDS_UPLOAD_LIMIT : constant         := 100;
   PARAMS_MAP_NAME          : constant String  := "attrs";
   Connection               : HTTP_Connection_Access;

   package JSON_Value_Vector is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                            Element_Type => JSON_Value);
   use JSON_Value_Vector;

   type Relation_Kind is (IS_ENCLOSED_IN,
                          IS_OF_TYPE,
                          CORRESPONDING_NAME_DEFINITION,
                          CORRESPONDING_INSTANCIATION,
                          CORRESPONDING_ASSIGNATION,
                          CORRESPONDING_FORMAL_NAME,
                          CORRESPONDING_ACTUAL_PARAMETER,
                          CORRESPONDING_PARAMETER_SPECIFICATION);

   protected Queue is
      entry Add (Element : JSON_Value);
      entry Get_Elements (Elements : out Vector);
      function Length return Natural;
      procedure Set_Exit_Process (Value : Boolean := True);
   private
      List : Vector := JSON_Value_Vector.Empty_Vector;
      Exit_Process : Boolean := False;
   end Queue;

   protected body Queue is
      entry Add (Element : JSON_Value)
        when List.Length < MAX_RECORDS_UPLOAD_LIMIT is
      begin
         List.Append (Element);
      end Add;

      entry Get_Elements (Elements : out Vector)
        when List.Length >= MIN_RECORDS_UPLOAD_LIMIT or Exit_Process
      is
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

   ---------
   -- Add --
   ---------

   procedure Add (Element : JSON_Value) is
   begin
      Queue.Add (Element);
   end;

   --------------------
   -- Create_Request --
   --------------------

   procedure Create_Request (Element : JSON_Value; Query : out Unbounded_String; Params : JSON_Value) is
      Filename : constant String  := Element.Get ("location").Get ("filename");
      Line     : constant Integer := Element.Get ("location").Get ("line");
      Column   : constant Integer := Element.Get ("location").Get ("column");

      Match_Clauses : Unbounded_String := Null_Unbounded_String;
      With_Clauses : Unbounded_String := Null_Unbounded_String;

      function To_Labels (Arr : JSON_Array) return String is
         S : constant String := GNATCOLL.JSON.Get (Get (Arr, 1));
         Result : Unbounded_String := """" & To_Unbounded_String (S) & """";
      begin
         for I in 2 .. Length (Arr) loop
            declare
               S : constant String := Get (Get (Arr, I));
            begin
               Result := Result & ", """ & To_Unbounded_String (S) & """";
            end;
         end loop;

         return "[" & To_String (Result) & "]";
      end To_Labels;

      procedure Pre_Relationship_Node (Key : String) is
         Elt_Key      : constant String := Key & "Elt";
         Node_Id_Key  : constant String := Key & "NodeId";
      begin
         if not Element.Has_Field (Key) or Element.Get (Key) = JSON_Null then
            return;
         end if;

         if Kind (Element.Get (Key).Get ("location").Get ("line")) = JSON_String_Type
         or (Element.Get (Key).Get ("location").Get ("filename") = Filename and then
           (
            Element.Get (Key).Get ("location").Get ("line") > Line or else
            Element.Get (Key).Get ("location").Get ("column") > Column
           )
         )
         then
            With_Clauses := With_Clauses & To_Unbounded_String
            ("MERGE (" & Elt_Key & ":" & Element.Get (Key).Get ("kind") & " { "
               & "node_id: $" & PARAMS_MAP_NAME & '.' & Node_Id_Key & " })" & ASCII.LF);
         else
            Match_Clauses := Match_Clauses & To_Unbounded_String
            ("MATCH (" & Elt_Key & ":" & Element.Get (Key).Get ("kind") & " { "
               & "node_id: $" & PARAMS_MAP_NAME & '.' & Node_Id_Key & " })" & ASCII.LF);
         end if;

         declare
            Id : constant String := Element.Get (Key).Get ("node_id");
         begin
            Params.Set_Field (Node_Id_Key,  Id);
         end;
      end Pre_Relationship_Node;

      procedure Post_Relationship_Node (Key : String; Relation : Relation_Kind) is
         procedure Set_Props (Field_Name : String; Elt : JSON_Value) is
         begin
            case Props_Kind'Value (Elt.Get ("_kind")) is
               when PK_Boolean =>
                  declare
                     Value : constant Boolean := Elt.Get ("value");
                  begin
                     Params.Set_Field (Field_Name, Value);
                  end;
               when PK_Integer =>
                  declare
                     Value : constant Integer := Elt.Get ("value");
                  begin
                     Params.Set_Field (Field_Name, Value);
                  end;
            end case;
         end Set_Props;

         Elt_Key        : constant String  := Key & "Elt";
         Relation_Props : Unbounded_String := To_Unbounded_String ("");
         Reverse_Relashionship : Boolean;
      begin -- Post_Relationship_Node
         if not Element.Has_Field (Key) or Element.Get (Key) = JSON_Null then
            return;
         end if;

         if Has_Field (Element.Get (Key), "properties") then
            --  This relation have properties
            declare
               Arr        : constant JSON_Array := Element.Get (Key).Get ("properties");
               Arr_Length : constant Natural    := Length (Arr);
            begin
               if Arr_Length = 0 then
                  goto SKIP_REL_PROPS;
               end if;

               declare
                  Elt : constant JSON_Value := Get (Arr, 1);
                  Key_Of_Elt : constant String := Elt.Get ("key");
                  Field_Name : constant Unbounded_String := Key & "_" & To_Unbounded_String (Key_Of_Elt);
               begin
                  Relation_Props := " { " & Key_Of_Elt & ": $" & PARAMS_MAP_NAME & '.' & Field_Name;
                  Set_Props (To_String (Field_Name), Elt);
               end;

               if Arr_Length > 1 then
                  for I in 2 .. Arr_Length loop
                     declare
                        Elt : constant JSON_Value := Get (Arr, I);
                        Key_Of_Elt : constant String := Elt.Get ("key");
                        Field_Name : constant Unbounded_String := Key & "_" & To_Unbounded_String (Key_Of_Elt);
                     begin
                        Relation_Props := Relation_Props & ", " & Key_Of_Elt & ": $" & PARAMS_MAP_NAME & '.'
                        & Field_Name;
                        Set_Props (To_String (Field_Name), Elt);
                     end;
                  end loop;
               end if;

               Relation_Props := Relation_Props & " }";
            end;
            <<SKIP_REL_PROPS>>
         end if;

         Reverse_Relashionship := Has_Field (Element.Get (Key), "reverse") and then Element.Get (Key).Get ("reverse");

         Query := Query & "MERGE (elt)"
           -- The relation between elt and Elt_Key
           & (if Reverse_Relashionship then "<" else "") & "-[:"
           & Relation_Kind'Image (Relation) & Relation_Props & "]-"
           & (if Reverse_Relashionship then "" else ">")
           -- Elt_Key node
           & "(" & Elt_Key & ")" & ASCII.LF;
      end Post_Relationship_Node;

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
                  Params.Set_Field (Key, Value);
               end;
            when JSON_Int_Type =>
               declare
                  Value : constant Integer := Element.Get (Key);
               begin
                  Params.Set_Field (Key, Value);
               end;
            when JSON_String_Type =>
               declare
                  Value : constant String := Element.Get (Key);
               begin
                  Params.Set_Field (Key, Value);
               end;
            when others =>
               Log.Error ("Error, JSON_Value_Type not supported: " &
                                    JSON_Value_Type'Image (Element.Get (Key).Kind));
         end case;

         Query := Query & ", " & Key & ": $" & PARAMS_MAP_NAME & '.' & Key;
      end Add_Property;

      Kinds : JSON_Array;
      Kind  : Unbounded_String := Null_Unbounded_String;
      nodeId  : constant String := Element.Get ("node_id");
   begin -- Create_Request
      if Element.Has_Field ("kinds") then
         Kinds := Element.Get ("kinds");
      end if;

      if Is_Empty (Kinds) then
         if not Element.Has_Field ("kind") then
            Log.Error ("No 'kind' nor 'kinds' found for node: " & Element.Write);
         end if;
         Append (Kinds, Element.Get ("kind"));
      end if;

      if not Element.Has_Field ("kind") then
         declare
            S : constant String := Get (Get (Kinds, Length (Kinds)));
         begin
            Kind := To_Unbounded_String (S);
         end;
      else
         declare
            K : constant String := Element.Get ("kind");
         begin
            Kind := To_Unbounded_String (K);
         end;
      end if;

      -- Get the Parent (Enclosing_Element) node
      Pre_Relationship_Node ("parent");

      -- Get the Type Declaration node
      Pre_Relationship_Node ("type_declaration");

      -- Get the Corresponding Name Definition Node
      Pre_Relationship_Node ("corresponding_name_definition");

      -- Get the corresponding instanciation
      Pre_Relationship_Node ("corresponding_instanciation");

      -- Get the corresponding assignation
      Pre_Relationship_Node ("corresponding_assignation");

      -- Get the corresponding formal name of a parameter association
      Pre_Relationship_Node ("corresponding_formal_name");

      -- Get the corresponding actual parameter of a parameter association
      Pre_Relationship_Node ("corresponding_actual_parameter");

      -- Get the corresponding parameter specification
      Pre_Relationship_Node ("corresponding_parameter_specification");

      -- The MERGE instruction CREATE the node if it doesn't exist, otherwise it MATCH (get) it.
      --
      -- First, we get the location of the node that we will add. An enclosing element can have the same location.
      Query := Match_Clauses & With_Clauses & Query
      -- The current (added) node
        & "MERGE (elt:" & Kind & " { node_id" & ": $" & PARAMS_MAP_NAME & '.' & "node_id })" & ASCII.LF;

      -- START: properties of the node that we add
      Query := Query & "SET elt += { kinds" & ": $" & PARAMS_MAP_NAME & '.' & "kinds, "
               & "filename: $" & PARAMS_MAP_NAME & '.' & "filename, line: $" & PARAMS_MAP_NAME & '.'
               & "line, column: $" & PARAMS_MAP_NAME & '.' & "column";
      Params.Set_Field ("node_id", nodeId);
      Params.Set_Field ("kinds", Kinds);
      -- Location params
      Params.Set_Field ("filename", Filename);
      Params.Set_Field ("line", Line);
      Params.Set_Field ("column", Column);

      -- Elements properties
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

      -- END: properties of the node that we add

      Query := Query & "}" & ASCII.LF;

      -- Create the relation between the current node and the Enclosing Element
      Post_Relationship_Node ("parent", IS_ENCLOSED_IN);

      -- Create the relation between the current node and the Type Declaration
      Post_Relationship_Node ("type_declaration", IS_OF_TYPE);

      -- Create the relation between the current node and the Corresponding Name Definition
      Post_Relationship_Node ("corresponding_name_definition", CORRESPONDING_NAME_DEFINITION);

      -- Create the relation between the current node and the corresponding instanciation
      Post_Relationship_Node ("corresponding_instanciation", CORRESPONDING_INSTANCIATION);

      -- Create the relation between the current node and the corresponding assignation
      Post_Relationship_Node ("corresponding_assignation", CORRESPONDING_ASSIGNATION);

      -- Create the relation between the current node and the corresponding formal name
      Post_Relationship_Node ("corresponding_formal_name", CORRESPONDING_FORMAL_NAME);

      -- Create the relation between the current node and the corresponding actual parameter
      Post_Relationship_Node ("corresponding_actual_parameter", CORRESPONDING_ACTUAL_PARAMETER);

      -- Create the relation between the current node (parameter association in a call)
      -- and the corresponding parameter specification
      Post_Relationship_Node ("corresponding_parameter_specification", CORRESPONDING_PARAMETER_SPECIFICATION);
      Query := Query & "WITH *" & ASCII.LF
               & "CALL apoc.create.setLabels(elt, " & To_Labels (Kinds)  &")" & ASCII.LF
               & "YIELD node" & ASCII.LF
               & "RETURN node;" & ASCII.LF;
   exception
      when E : others =>
         Log.Error ("Error on creating request with: " & Element.Write, E, Trace => True);
         raise;
   end Create_Request;

   --------------
   -- Watchdog --
   --------------

   task Watchdog is
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
      Upload_Manager.Start;
      loop
         select
            accept Finalize;
            exit;
         or accept Alive;
         end select;
      end loop;
   end Watchdog;

   task body Upload_Manager is
      use Ada.Text_IO;
      procedure Open_File (File      : in out File_Type;
                           Mode      : File_Mode;
                           Path      : String;
                           File_Form : String  := "wcem=8";
                           Auto      : Boolean := True)
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

      procedure Send_Query (Query : String) is
         use AWS.Messages;

         Response : AWS.Response.Data;
      begin
         if DRY_RUN then
            return;
         end if;

         case NEO4J_MODE is
            when File_Neo4j_Mode =>
               --  Nothing to do here.
               null;
            when HTTP_Neo4j_Mode =>
               -- Sends the request to the server
               declare
                  use Ada.Streams;
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
                  Post (Connection.all,
                        Response,
                        Queries_Stream,
                        "application/json");
               end;

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
                     Errors_Array  : constant JSON_Array := Response_JSON.Get("errors");
                     Errors        : Unbounded_String    := Null_Unbounded_String;

                  begin
                     if not GNATCOLL.JSON.Is_Empty (Errors_Array) then
                        for Index in Positive'First .. GNATCOLL.JSON.Length (Response_JSON.Get("errors"))
                        loop
                           declare
                              code : constant String := Get (Errors_Array, Index).Get ("code");
                              message : constant String := Get (Errors_Array, Index).Get ("message");
                           begin
                              Errors := Errors & To_Unbounded_String (code) & ": "
                                 & To_Unbounded_String (message)
                                 & ASCII.LF;
                           end;
                        end loop;
                        Log.Error ("End response errors: {0}" & ASCII.LF
                        & "Request: {1}", To_String (Errors), Query);
                     end if;
                  end;
               when Client_Error | Server_Error =>
                  Log.Error ("Start Client_Error | Server_Error");
                  Log.Error
                     ("Upload_Manager error in HTTP response: " &
                        Reason_Phrase (AWS.Response.Status_Code (Response)));
                  raise Program_Error with Reason_Phrase (AWS.Response.Status_Code (Response));
               when others =>
                  Log.Info ("Other response code");
                  -- Other HTTP response code are not important
                  null;
               end case;
               Log.Debug ("End Request");
            null;
         end case;
      end Send_Query;

      Req_File_Path  : constant String := Ada.Directories.Compose (Ada.Directories.Current_Directory, "req.json");
      DB_File_Path   : constant String :=
         Ada.Directories.Compose (Ada.Directories.Current_Directory, "database.cypher");
      Exit_Loop      : Boolean         := False;
      File, File_Db  : File_Type;
      Is_First_Debug : Boolean         := True;
   begin -- Upload_Manager
      Log.Debug ("[Upload_Manager] Start");
      if DEBUG and Ada.Directories.Exists (Req_File_Path) then
         Ada.Directories.Delete_File (Req_File_Path);
      end if;

      if NEO4J_MODE = File_Neo4j_Mode then
         Log.Debug ("[Upload_Manager] File MODE");
         if Ada.Directories.Exists (DB_File_Path) then
            Ada.Directories.Delete_File (DB_File_Path);
         end if;
         Open_File (File => File_Db,
            Mode => Append_File,
            Path => DB_File_Path);
         Log.Debug ("[Upload_Manager] File created");
      end if;

      accept Start;

      if NEO4J_MODE = HTTP_Neo4j_Mode then
         declare
            --  This type allows to show a Debug message every second, instead of every 100ms
            type Sec is mod 10;

            Exit_Loop : Boolean := False;
            Counter   : Sec := Sec'First;
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
         end;
      end if;

      Add_Indexes : declare
         Request    : constant JSON_Value := Create_Object;
         Statement  : constant JSON_Value := Create_Object;
         Statements : JSON_Array;
      begin
         Statement.Set_Field ("statement",
                              "CREATE INDEX nodeIdIndex IF NOT EXISTS FOR (n:Element) ON (n.node_id);" & ASCII.LF);
         Append (Statements, Statement);
         Request.Set_Field ("statements", Statements);

         if NEO4J_MODE = File_Neo4j_Mode then
            Put_Line (File_Db, To_String (Statement.Get ("statement")));
         end if;

         Send_Query (Request.Write);
      end Add_Indexes;

      loop
         -- We create the HTTP request
         HTTP_Request : declare
            Request    : constant JSON_Value := Create_Object;
            Req_UStr   : Ada.Strings.Unbounded.Unbounded_String;
            Statements : JSON_Array;

            Elements : Vector;
         begin
            Queue.Get_Elements (Elements);

            Add_Cypher_Queries : for Elt : JSON_Value of Elements loop
               Cypher_Query : declare
                  Query     : Unbounded_String    := To_Unbounded_String ("");
                  Params    : constant JSON_Value := Create_Object;
                  Attrs     : constant JSON_Value := Create_Object;
                  Statement : constant JSON_Value := Create_Object;
               begin
                  Create_Request (Element => Elt,
                                  Query   => Query,
                                  Params  => Params);
                  Statement.Set_Field ("statement", To_String (Query));
                  Attrs.Set_Field ("attrs", Params);
                  Statement.Set_Field ("parameters", Attrs);
                  Append (Statements, Statement);

                  if NEO4J_MODE = File_Neo4j_Mode then
                     declare
                        Needs_Comma : Boolean := False;
                        procedure Json_To_Cypher_Param (Name : UTF8_String; Value : JSON_Value) is
                        begin
                           Put (File_Db, (if Needs_Comma then "," else "") & Name & ":" & Value.Write);
                           if not Needs_Comma then
                              Needs_Comma := True;
                           end if;
                        end Json_To_Cypher_Param;
                     begin
                        -- Put_Line (File_Db, ":param " & PARAMS_MAP_NAME & " => " & Params.Write & ";");
                        Put (File_Db, ":param " & PARAMS_MAP_NAME & " => {");
                        Params.Map_JSON_Object (Json_To_Cypher_Param'Access);
                        Put_Line (File_Db, "};");
                     end;
--                       Put_Line (File_Db, ":params " & To_String (Params.Write));
                     Put_Line (File_Db, To_String (Query));
                  end if;
               end Cypher_Query;
            end loop Add_Cypher_Queries;

            Request.Set_Field ("statements", Statements);

            Req_UStr := Request.Write;

            declare
               -- The use of variable for the conversion to String is to prevent stackoverflow due to heap size
               -- and conversion implementation depending on the OS and the CPU architecture.
               --
               -- The none use of variable raise:
               -- - STORAGE_ERROR : stack overflow
               --
               -- When the string request is huge.
               Queries_Str : constant String := To_String (Req_UStr);
            begin
               -- Write request in file in DEBUG mode
               if DEBUG then
                  declare
                  begin
                     Open_File (File => File,
                                Mode => Append_File,
                                Path => Req_File_Path);

                     Put (File => File,
                          Item => (if Is_First_Debug then "[" & ASCII.LF else ","));
                     Put_Line (File, Queries_Str);
                     Close (File);
                     Is_First_Debug := False;
                  exception
                     when E : others =>
                        Log.Error ("Error on writing log file:", E, Trace => True);
                        Close (File);
                  end;
               end if;

               Send_Query (Queries_Str);
            end;
         end HTTP_Request;

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

      -- In DEBUG mode: end of JSON log file
      if DEBUG then
         declare
         begin
            Open_File (File => File,
                       Mode => Append_File,
                       Path => Req_File_Path);

            Put_Line (File, "]");
            Close (File);
         exception
            when E : others =>
               Log.Error ("Error when ending log file:", E, Trace => True);
         end;
      end if;

      Add_Order_Of_Nodes : declare
         Request    : constant JSON_Value := Create_Object;
         Statement  : constant JSON_Value := Create_Object;
         Statements : JSON_Array;
      begin
         Statement.Set_Field
           ("statement", "MATCH (p)<-[r:IS_ENCLOSED_IN]-(e:Element)" & ASCII.LF
            & "  WHERE r.index IS NULL" & ASCII.LF
            & "WITH p, e, r" & ASCII.LF
            & "  ORDER BY e.filename, e.line, e.column" & ASCII.LF
            & "CALL {" & ASCII.LF
            & "WITH p" & ASCII.LF
            & "MATCH (p)<-[r2:IS_ENCLOSED_IN]-(:Element)" & ASCII.LF
            & "  WHERE r2.index IS NOT NULL" & ASCII.LF
            & "RETURN count(r2) as nb" & ASCII.LF
            & "}" & ASCII.LF
            & "WITH p, nb, collect(distinct r) as rels" & ASCII.LF
            & "FOREACH (n IN rels | SET n.index = apoc.coll.indexOf(rels, n) + nb + 1);");
         Append (Statements, Statement);
         Request.Set_Field ("statements", Statements);

         if NEO4J_MODE = File_Neo4j_Mode then
            Put_Line (File_Db, To_String (Statement.Get ("statement")));
         end if;

         Send_Query (Request.Write);
      end Add_Order_Of_Nodes;

      if NEO4J_MODE = File_Neo4j_Mode then
         Close (File_Db);
      end if;
      Log.Debug ("[Upload_Manager] End");
   exception
      when E : others =>
         Log.Debug ("[Upload_Manager] Exception: {0}", Ada.Exceptions.Exception_Name (E));
         Log.Debug ("[Upload_Manager] error: {0}", Ada.Exceptions.Exception_Information (E));
         if NEO4J_MODE = File_Neo4j_Mode then
            Close (File_Db);
         end if;
         Log.Error ("Upload_Manager error: ", E, Trace => True);
   end Upload_Manager;

   procedure Finalize is
   begin
      Queue.Set_Exit_Process;
      Watchdog.Finalize;
   end Finalize;
begin
   Log.Debug ("NEO4J_MODE: {0}", Neo4j_Mode_Type'Image (NEO4J_MODE));

   if NEO4J_MODE = HTTP_Neo4j_Mode then
      if Ada.Environment_Variables.Exists ("NEO4J_HOST")
      and Ada.Environment_Variables.Exists ("NEO4J_USER")
      and Ada.Environment_Variables.Exists ("NEO4J_PASS")
      then
         Connection := new HTTP_Connection '(Create (Host => Ada.Environment_Variables.Value ("NEO4J_HOST"),
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
end neo4j_wrapper;
