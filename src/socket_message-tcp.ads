with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets; use GNAT.Sockets;

generic
   Host : Unbounded_String := To_Unbounded_String ("127.0.0.1");
   Port : Port_Type        := 8001;
package Socket_Message.TCP is

   --
   -- This procedure sends out a single character to the socket. It
   -- will not wait for the socket ready by default.
   --
   procedure Put (Char : in Character; Blocking : Boolean := False);
   
   --
   -- This procedure sends out a text string to the socket. It
   -- will not wait for the socket ready by default.
   --
   procedure Put (Text : in String; Blocking : Boolean := False);
   
   --
   -- This procedure sends out a complete line of text to the socket.
   -- It will append a CR and LF character to the output string. It 
   -- will not wait for the socket ready by default.
   --
   procedure Put_Line (Text : in String; Blocking : Boolean := False);
   
   --
   -- Reads a character from the socket. The procedure will not
   -- wait for a character, if there is no character ready to be
   -- read the procedure returns a status of No_Char.
   --
   procedure Read (Char : out Character; Result : out Status);
   
   --
   -- Read a character form the socket, the procedure will wait for
   -- an incoming character.
   -- 
   procedure Get (Char : out Character; Result : out Status; Blocking : Boolean := True);
   
   --
   -- Read a line of text from the socket, a line is a string 
   -- terminated by a CR.
   --
   procedure Get_Line (Text : out String; Length : out Integer; Result : out Status; Blocking : Boolean := True);
   
   -- Must be called at the end of the program to prevent non stopping
   -- program when no client connected to the socket.
   procedure Finalize;

end Socket_Message.TCP;
