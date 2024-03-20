pragma Ada_2012;

with Ada.Containers.Vectors;
with Ada.Containers;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;


with Load_Environment_Variables;
with Util.Log.Loggers;

package body Export_Manager.JSONL is
   use type Ada.Containers.Count_Type;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Export_Manager.JSONL");

   MAX_RECORDS_UPLOAD_LIMIT : constant        := 1_000;
   MIN_RECORDS_UPLOAD_LIMIT : constant        := 100;

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

      entry Get_Elements (Elements : out Vector)
        when Add'Count = 0 or List.Length >= MIN_RECORDS_UPLOAD_LIMIT or Exit_Process is
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
         end select;
      end loop;
   end Watchdog;

   task body Upload_Manager is
      use Ada.Text_IO;
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

      DB_File_Path   : constant String := Ada.Directories.Compose (Ada.Directories.Current_Directory, "database.jsonl");
      Exit_Loop      : Boolean         := False;
      File_Db  : File_Type;
   begin -- Upload_Manager
      select
         accept Start;
      or
         terminate;
      end select;
      Log.Debug ("[Upload_Manager] File MODE");
      if Ada.Directories.Exists (DB_File_Path) then
         Ada.Directories.Delete_File (DB_File_Path);
      end if;
      Open_File (File => File_Db, Mode => Append_File, Path => DB_File_Path);
      Log.Debug ("[Upload_Manager] File created");

      loop
         -- We create the HTTP request
         HTTP_Request :
         declare
            Elements : Vector;
         begin
            Queue.Get_Elements (Elements);

            Add_Cypher_Queries :
            for Elt : JSON_Value of Elements loop
               Put_Line (File_Db, Elt.Write);
            end loop Add_Cypher_Queries;
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

      Close (File_Db);

      Log.Debug ("[Upload_Manager] End");
   exception
      when E : others =>
         Log.Debug ("[Upload_Manager] Exception: {0}", Ada.Exceptions.Exception_Name (E));
         Log.Debug ("[Upload_Manager] error: {0}", Ada.Exceptions.Exception_Information (E));
         Close (File_Db);
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
               Log.Debug ("[Instance] Finilize end");
            end Finalize;
         end select;
      end loop;
   end Instance;

   function Initialize return Observer_Type is
   begin
      -- TODO: create files, etc.
      Watchdog.Initialize;

      return (Name => To_Unbounded_String ("JSON"), Job => new Instance);
   end Initialize;
end Export_Manager.JSONL;
