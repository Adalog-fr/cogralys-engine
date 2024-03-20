with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with GNAT.OS_Lib;
with System.OS_Constants;

package body Socket_Message.TCP is
   use Ada.Streams, Ada.Strings.Unbounded;
   use type Ada.Streams.Stream_Element_Offset;

   package LAT1 renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------------------------------------------------------
   -- SPECIFICATION
   --------------------------------------------------------------------------------------------------------------------

   No_Data : exception;

   subtype Stream_Type is Ada.Streams.Stream_Element_Array (1 .. 1);

   -- Manage the data send to client.
   protected type Message_Manager_Type is
      -- Called by the app/task when it want to read data
      procedure Set_Can_Add (Value : Boolean);
      -- Used by the app/task to add data
      entry Write (Char : Character);
      -- Used by the app/task to send data through a socket
      entry Read (Char : out Character);
      -- Used by the app/task to know if there is data to send
      function Have_Data return Boolean;
   private
      Can_Add     : Boolean := True;
      Stream      : Unbounded_String := Null_Unbounded_String;
      Last        : Ada.Streams.Stream_Element_Offset;
   end Message_Manager_Type;

   Message_Manager          : Message_Manager_Type;
   Received_Message_Manager : Message_Manager_Type;

   protected Task_Termination is
      procedure Terminate_Program;
      function Is_Program_Termination return Boolean;
   private
      Is_Terminated : Boolean := False;
   end Task_Termination;

   Client : aliased Socket_Type;
   task type Read_Char (Channel : Stream_Access) is
   end Read_Char;
   type Read_Char_Access is access Read_Char;

   Reader : Read_Char_Access;

   task Socket_Manager is
      -- Called when the app want to Put a character in blocking mode
      entry Put;
      -- Called when the app want to get a character in non-blocking mode
      entry Read (Char : out Character; Have_Data : out Boolean);
      -- Called when the app want to get a character in blocking mode
      entry Get  (Char : out Character);
      -- Called when the app want to Put a character in non-blocking mode
      entry Signal;
   end Socket_Manager;

   --------------------------------------------------------------------------------------------------------------------
   -- HIGH LEVEL API
   --------------------------------------------------------------------------------------------------------------------

   ---------
   -- Put --
   ---------

   procedure Put (Char : in Character; Blocking : Boolean := False) is
   begin
      Message_Manager.Set_Can_Add (True);
      Message_Manager.Write (Char);
      Message_Manager.Set_Can_Add (False);

      if Blocking then
         Socket_Manager.Put;
      else
         select
            Socket_Manager.Signal;
         else null;
         end select;
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Text : in String; Blocking : Boolean := False) is
   begin
      for Char of Text loop
         Put (Char, Blocking);
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Text : in String; Blocking : Boolean := False) is
   begin
      Put (Text, Blocking);
      Put (LAT1.CR, Blocking);
      Put (LAT1.LF, Blocking);
   end Put_Line;

   ---------------
   -- Read_Char --
   ---------------

   procedure Read (Char : out Character; Result : out Status) is
      Tmp_Char  : Character;
      Have_Data : Boolean;
   begin
      select
         Socket_Manager.Read (Tmp_Char, Have_Data);
         if Have_Data then
            if Character'Pos (Tmp_Char) > 127 then
               Result := Bad_Char;
            else
               Char   := Tmp_Char;
               Result := Success;
            end if;
         else
            Result := No_Char;
         end if;
      else
         Result := No_Char;
      end select;
   end Read;

   ---------
   -- Get --
   ---------

   procedure Get (Char : out Character; Result : out Status; Blocking : Boolean := True) is
      Tmp_Char : Character;
      Found    : Boolean;
   begin
      if Blocking then
         Socket_Manager.Get (Tmp_Char);
      else
         Socket_Manager.Read (Tmp_Char, Found);
      end if;

      if not Blocking and not Found then
         Result := No_Char;
         return;
      end if;

      if Character'Pos (Tmp_Char) > 127 then
         Result := Bad_Char;
      else
         Char   := Tmp_Char;
         Result := Success;
      end if;
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (Text : out String; Length : out Integer; Result : out Status; Blocking : Boolean := True)
   is
      Local_Result : Status;
      Char : Character;
      Count : Integer := 0;
      Exit_Loop : Boolean := False;
   begin
      Get (Char, Local_Result, Blocking);

      while Char /= LAT1.CR and Char /= LAT1.LF loop

         if Local_Result = No_Char then
            Exit_Loop := True;
            Result    := No_Char;
            exit;
         end if;

         Count := Count + 1;
         Text (Count) := Char;
         if Count = Text'Last then
            Length := Text'Last;
            Result := Success;
            Exit_Loop := True;
         end if;
         exit when Exit_Loop;
         Get (Char, Local_Result);
      end loop;
      if not Exit_Loop then
         Length := Count;
         Result := Success;
      end if;
   end Get_Line;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin

      if Reader /= null then
         abort Reader.all;
      end if;

      Task_Termination.Terminate_Program;

   end Finalize;

   --------------------------------------------------------------------------------------------------------------------
   -- LOW LEVEL API
   --------------------------------------------------------------------------------------------------------------------

   ---------------------
   -- Message_Manager --
   ---------------------

   protected body Message_Manager_Type is
      procedure Set_Can_Add (Value : Boolean) is
      begin
         Can_Add := Value;
      end Set_Can_Add;

      entry Write (Char : Character) when Can_Add is
      begin
         Append (Stream, Char);
      end Write;

      entry Read (Char : out Character) when not Can_Add is
      begin
         if Length (Stream) = 0 then
            raise No_Data;
         end if;
         Char := Element (Stream, 1);
         Delete (Stream, 1, 1);
      end Read;

      function Have_Data return Boolean is
      begin
         return Length (Stream) > 0;
      end Have_Data;
   end Message_Manager_Type;

   ----------------------
   -- Task_Termination --
   ----------------------

   protected body Task_Termination is
      procedure Terminate_Program is
      begin
         Is_Terminated := True;
      end Terminate_Program;

      function Is_Program_Termination return Boolean is
      begin
         return Is_Terminated;
      end Is_Program_Termination;
   end Task_Termination;

   ---------------
   -- Read_Char --
   ---------------

   task body Read_Char is
      Message                : Stream_Type;
      Last                   : Ada.Streams.Stream_Element_Offset;
   begin
      Infinite_Read : loop
         Channel.Read (Item => Message,
                       Last => Last);
         if Last /= 0 then
            Received_Message_Manager.Set_Can_Add (True);
            Received_Message_Manager.Write (Character'Val (Integer (Message (1))));
            Received_Message_Manager.Set_Can_Add (False);
         else
            Close_Socket (Client);
            exit;
         end if;
      end loop Infinite_Read;
   end Read_Char;

   --------------------
   -- Socket_Manager --
   --------------------

   task body Socket_Manager is
      Server                 : Socket_Type;
      Src                    : Sock_Addr_Type;
      Channel                : Stream_Access;
      Socket_Status          : Selector_Status := Aborted;
      Wait_Accept_Connection : Boolean         := True;
      Task_Char              : Character;
   begin
      Create_Socket (Server);
      Bind_Socket (Socket  => Server,
                   Address => (Family => Family_Inet,
                               Addr   => Inet_Addr (To_String (Host)),
                               Port   => Port));
      Listen_Socket (Socket =>  Server);

      loop
         <<CONTINUE>>

         if Task_Termination.Is_Program_Termination then
            exit;
         end if;

         -- Wait for a connection if no client connected. It is a non-blocking wait.
         -- It will wait 1 second for a connection.
         if Wait_Accept_Connection then
            Accept_Socket (Server  => Server,
                           Socket  => Client,
                           Address => Src,
                           Timeout => Duration (1.0),
                           Status  => Socket_Status);

            if Socket_Status = Completed then
               declare
                  Socket_Request : Request_Type := Request_Type '(Non_Blocking_IO, True);
               begin
                  Control_Socket (Socket  => Server,
                                  Request => Socket_Request);
               end;
               Channel                := GNAT.Sockets.Stream (Client);
               Wait_Accept_Connection := False;
               Reader := new Read_Char (Channel);
            end if;
         end if;

         -- If no client, no need to go any further
         if Wait_Accept_Connection then
            goto CONTINUE;
         end if;

         -- Send message to the client
         Message_Manager.Set_Can_Add (False);
         Send_Data : while Message_Manager.Have_Data loop
            Message_Manager.Read (Task_Char);
            begin
               Character'Output (Channel, Task_Char);
            exception
               when GNAT.SOCKETS.SOCKET_ERROR =>
                  if GNAT.OS_Lib.Errno in System.OS_Constants.EPIPE | System.OS_Constants.EBADF then
                     -- Client left

                     Wait_Accept_Connection := True;
                     exit Send_Data;
                  else
                     raise;
                  end if;
            end;
         end loop Send_Data;
         Message_Manager.Set_Can_Add (True);

         if Wait_Accept_Connection then
            goto CONTINUE;
         end if;

         -- Blocking operations
         select
            accept Put;
         or accept Read (Char : out Character; Have_Data : out Boolean) do
               if not Received_Message_Manager.Have_Data then
                  Have_Data := False;
                  return;
               end if;
               Received_Message_Manager.Read (Char);
               Have_Data := True;
            end Read;
         or accept Signal;
         or
            accept Get (Char : out Character) do
               declare
                  Found : Boolean := False;
               begin
                  while not Found loop
                     if Received_Message_Manager.Have_Data then
                        Received_Message_Manager.Read (Char);
                        Found := True;
                     else
                        delay Duration (0.5);
                     end if;
                  end loop;
               end;
            end Get;
         or terminate;
         end select;
      end loop;

      Close_Socket (Client);
      Close_Socket (Server);
   end Socket_Manager;

end Socket_Message.TCP;
