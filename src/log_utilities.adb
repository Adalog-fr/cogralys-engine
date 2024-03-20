with Ada.Characters.Handling,
     Ada.Exceptions,
     Ada.Strings.Unbounded;

with Asis, Asis.Compilation_Units, Asis.Elements, Asis.Text;

with A4G_Bugs;

with Thick_Queries,
     Utilities;

package body Log_Utilities is
    use Ada.Strings.Unbounded;
    use Utilities;
    use Asis, Asis.Compilation_Units, Asis.Elements, Asis.Text;

    package ACH renames Ada.Characters.Handling;

    ----------------
    -- Trace_Elem --
    ----------------

    function Trace_Elem (Element : Asis.Element) return String is
        Result : Unbounded_String := Null_Unbounded_String;

        S : constant Span := A4G_Bugs.Element_Span (Element);
    begin
        Append (Result, Element_Kinds'Image (Element_Kind (Element)));
        if not Is_Nil (Element) then
            Append (Result, " => ");
            case Element_Kind (Element) is
                when Not_An_Element =>
                    -- Impossible actually, but we don't feel like calling Failure from here
                    null;
                when A_Pragma =>
                    Append (Result, Pragma_Kinds'Image (Pragma_Kind (Element)));
                when A_Defining_Name =>
                    Append (Result, Defining_Name_Kinds'Image (Defining_Name_Kind (Element)));
                when A_Declaration =>
                    Append (Result, Declaration_Kinds'Image (Declaration_Kind (Element)));
                when A_Definition =>
                    Append (Result, Definition_Kinds'Image (Definition_Kind (Element)));
                when An_Expression =>
                    Append (Result, Expression_Kinds'Image (Expression_Kind (Element)));
                when An_Association =>
                    Append (Result, Association_Kinds'Image (Association_Kind (Element)));
                when A_Statement =>
                    Append (Result, Statement_Kinds'Image (Statement_Kind (Element)));
                when A_Path =>
                    Append (Result, Path_Kinds'Image (Path_Kind (Element)));
                when A_Clause =>
                    Append (Result, Clause_Kinds'Image (Clause_Kind (Element)));
                when An_Exception_Handler =>
                    null;
            end case;

            Append (Result, " at ");
            Append (Result, ACH.To_String (Text_Name (Enclosing_Compilation_Unit (Element))));
            Append (Result, ':');
            Append (Result, ACH.To_String (ASIS_Integer_Img (S.First_Line)));
            Append (Result, ':');
            Append (Result, ACH.To_String (ASIS_Integer_Img (S.First_Column)));
        end if;
        Append (Result, ASCII.LF);

        return To_String (Result);
    end Trace_Elem;

    procedure Print
       (Log : Logger'Class; Level : Level_Type; Message : String; Element : Asis.Element; With_Source : Boolean := True)
    is
        use Ada.Exceptions;
        use Thick_Queries;

        Result : Unbounded_String := Null_Unbounded_String;
    begin

        Append (Result, Message);
        Append (Result, Trace_Elem (Element));

        if With_Source and not Is_Nil (Element) then
            if Is_Part_Of_Implicit (Element) then
               Append (Result, "(implicit) "
                & ACH.To_String (Extended_Name_Image (Element, Silent_If_Inappropriate => True)));
            elsif Is_Part_Of_Instance (Element) then
               Append (Result, "(from instance) "
               & ACH.To_String (Extended_Name_Image (Element, Silent_If_Inappropriate => True)));
            else
               begin
                  Append (Result, ACH.To_String (Element_Image (Element)));
               exception
                  when Occur : others =>   -- Happens sometimes in case of ASIS bugs...
                     Append (Result, "Image unavailable: " & Exception_Name (Occur));
               end;
            end if;
         end if;

        Log.Print (Level => Level, Message => To_String (Result));
    end Print;

    procedure Debug (Log : Logger'Class; Message : String; Element : Asis.Element; With_Source : Boolean := True) is
    begin
        Print (Log, DEBUG_LEVEL, Message, Element, With_Source);
    end Debug;

    procedure Info (Log : Logger'Class; Message : String; Element : Asis.Element; With_Source : Boolean := True) is
    begin
        Print (Log, INFO_LEVEL, Message, Element, With_Source);
    end Info;

    procedure Warn (Log : Logger'Class; Message : String; Element : Asis.Element; With_Source : Boolean := True) is
    begin
        Print (Log, WARN_LEVEL, Message, Element, With_Source);
    end Warn;

    procedure Error (Log : Logger'Class; Message : String; Element : Asis.Element; With_Source : Boolean := True) is
    begin
        Print (Log, ERROR_LEVEL, Message, Element, With_Source);
    end Error;
end Log_Utilities;
