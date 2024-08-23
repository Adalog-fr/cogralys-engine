----------------------------------------------------------------------
--  Framework.Plugs - Package body                                  --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2014.           --
--  The Ada Controller is  free software; you can  redistribute  it --
--  and/or modify it under  terms of the GNU General Public License --
--  as published by the Free Software Foundation; either version 2, --
--  or (at your option) any later version. This unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from the units  of this program, or if you  link this unit with --
--  other files  to produce  an executable, this  unit does  not by --
--  itself cause the resulting executable  to be covered by the GNU --
--  General  Public  License.   This  exception  does  not  however --
--  invalidate any  other reasons why the executable  file might be --
--  covered by the GNU Public License.                              --
----------------------------------------------------------------------

-- Ada
with Ada.Characters.Conversions,
     Ada.Characters.Handling,
     Ada.Containers.Hashed_Maps,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Hash,
     Ada.Strings.Wide_Fixed;

-- ASIS
with A4G.A_Types,
     Asis,
     Asis.Compilation_Units,
     Asis.Declarations,
     Asis.Definitions,
     Asis.Elements,
     Asis.Exceptions,
     Asis.Expressions,
     Asis.Extensions.Strings,
     Asis.Set_Get,
     Asis.Statements,
     Asis.Text;

-- Misc
with Export_Manager,
     Export_Utils,
     GNAT.Regpat,
     GNATCOLL.JSON,
     Log_Utilities,
     Thick_Queries,
     Utilities,
     Util.Log.Loggers;

package body Framework.Plugs is
   use Utilities;
   use A4G.A_Types;
   use Ada.Containers;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Framework.Plugs");

   package Config_File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Boolean,
      Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");
   use Config_File_Maps;

   Config_File_Map : Map := Empty_Map;

   MAX_CHARACTERS_IN_CONTENT : constant := 200;

   function Content_Substr (Str : String) return String is
   begin
      if Str'Length > MAX_CHARACTERS_IN_CONTENT then
         return Str (Str'First .. MAX_CHARACTERS_IN_CONTENT - 3) & "...";
      end if;
      return Str;
   end Content_Substr;

   ---------------------------
   -- Is_In_Predefined_Unit --
   ---------------------------

   function Is_In_Predefined_Unit (E : Asis.Element) return Boolean is
      use Asis, Asis.Compilation_Units, Asis.Elements;
      CU : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (E);
      UO : constant Asis.Unit_Origins := Unit_Origin (CU);
   begin
      return UO = A_Predefined_Unit;
   end Is_In_Predefined_Unit;

   ------------------------
   -- Get_Fixed_Location --
   ------------------------

   function Get_Fixed_Location (E : Asis.Element) return Location is
      use Asis, Asis.Compilation_Units, Asis.Elements, Asis.Text;

      Result : Location := Get_Location (E);
   begin
      if Result /= Null_Location or Is_Nil (E) then
         return Result;
      end if;

      if Association_Kind (E) = A_Parameter_Association then
         -- This case is when we explore childs of a call, because we explore the normalized form.
         declare
            use Asis.Expressions;
            Params : constant Asis.Association_List := Thick_Queries.Actual_Parameters
              (Element    => Enclosing_Element (E),
               Normalized => True);

            Index : Positive := 1;
         begin
            Result := Get_Fixed_Location (Enclosing_Element (E));
            for Elt : Asis.Association of Params loop
               if Is_Equal (Formal_Parameter (E), Formal_Parameter (Elt)) then
                  return Create_Location (File         => Get_File_Name (Result),
                                          First_Line   => Get_First_Line (Result),
                                          First_Column => Index * (-1));
               end if;
               Index := Index + 1;
            end loop;
         end;
      end if;

      declare
         CU : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (E);
         UO : constant Asis.Unit_Origins := Unit_Origin (CU);
      begin
         if UO = A_Predefined_Unit then
            --  Ada predefined language environment units listed in Annex A(2).
            --  These include Standard and the three root library units: Ada, Interfaces, and System, and their
            --  descendants.  i.e., Ada.Text_Io, Ada.Calendar, Interfaces.C, etc.

            Result := Create_Location (File         => Unit_Full_Name (CU),
                                       First_Line   => 0,
                                       First_Column => 0);
         else
            declare
               use GNAT.Regpat;

               S           : constant String                      := Asis.Extensions.Strings.Build_GNAT_Location (E);
               RE_LOCATION : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("^([^:]+):(\d+):(\d+)");

               Result_Array : Match_Array (0 .. 3);
               Line         : Line_Number;
               Column       : Character_Position;
            begin
               GNAT.Regpat.Match (RE_LOCATION, S, Result_Array);

               Line   := Line_Number'Value (S (Result_Array (2).First .. Result_Array (2).Last));
               Column := Character_Position'Value (S (Result_Array (3).First .. Result_Array (3).Last));
               Result := Create_Location (File         => Text_Name (CU),
                                          First_Line   => Line,
                                          First_Column => Column);
            exception
               when others =>
                  Utilities.Failure (Message => "Unknown location of",
                                     Element => E);

            end;
         end if;
         return Result;
      end;
   end Get_Fixed_Location;

   ------------------------
   -- Get_Fixed_Location --
   ------------------------

   function Get_Fixed_Location (E : Asis.Element) return GNATCOLL.JSON.JSON_Value is
      use Ada.Characters.Handling,
          Asis,
          Asis.Compilation_Units,
          Asis.Declarations,
          Asis.Elements,
          GNATCOLL.JSON,
          Thick_Queries;
      Json_Def_Location : constant JSON_Value := Create_Object;
      Loc               : constant Location   := Get_Fixed_Location (E);
   begin
      if Is_In_Predefined_Unit (E) and then Get_First_Line (Loc) = 0 then
         declare
            use Asis.Statements;
            CU        : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (E);
            Unit_Name : constant String                := To_String (Unit_Full_Name (CU));
         begin
            Json_Def_Location.Set_Field ("filename", Unit_Name);
            Json_Def_Location.Set_Field ("column", Natural (0));

            case Element_Kind (E) is
               when A_Declaration =>
                  Json_Def_Location.Set_Field ("line", To_String (Full_Name_Image (Names (E) (1))));
               when A_Statement =>
                  Json_Def_Location.Set_Field ("line", To_String (Full_Name_Image (Label_Names (E) (1))));
               when An_Expression =>
                  case Expression_Kind (E) is
                     when An_Integer_Literal | A_Real_Literal | A_String_Literal =>
                        Json_Def_Location.Set_Field ("line", To_String (Asis.Expressions.Value_Image (E)));
                     when An_Identifier | A_Selected_Component | An_Attribute_Reference =>
                        Json_Def_Location.Set_Field ("line", To_String (Full_Name_Image (E)));
                     when others =>
                        Log.Warn ("Getting line location of "& Expression_Kinds'Image (Expression_Kind (E)) &
                        " in a predefined unit is currently not implemented");
                        Json_Def_Location.Set_Field ("line",
                        To_String (Text_Name (Enclosing_Compilation_Unit (E))) & ":" &
                        Expression_Kinds'Image (Expression_Kind (E)) & ":" &
                        To_String (Asis.Elements.Debug_Image (E)));
                  end case;
               when others =>
                  Json_Def_Location.Set_Field ("line", To_String (Full_Name_Image (E)));
            end case;
         end;
         return Json_Def_Location;
      end if;

      Json_Def_Location.Set_Field ("filename", To_String (Get_File_Name (Loc)));
      Json_Def_Location.Set_Field ("line", Get_First_Line (Loc));
      Json_Def_Location.Set_Field ("column", Get_First_Column (Loc));

      return Json_Def_Location;
   end Get_Fixed_Location;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (CU : Asis.Compilation_Unit) return GNATCOLL.JSON.JSON_Value is
      use Ada.Characters.Handling,
          Asis,
          Asis.Compilation_Units,
          Asis.Declarations,
          Asis.Elements,
          GNATCOLL.JSON,
          Thick_Queries;

      Json_Def_Location : constant JSON_Value := Create_Object;
      File_Name         : constant String := To_String (Text_Name (CU));
      Index             : constant Natural := 0;
   begin
      Json_Def_Location.Set_Field ("filename", File_Name);
      Json_Def_Location.Set_Field ("line", Index);
      Json_Def_Location.Set_Field ("column", Index);

      return Json_Def_Location;
   end Get_Location;


   --------------------
   -- Get_Kind_Image --
   --------------------

   function Get_Kind_Image (Element : Asis.Element) return String is
      use Asis, Asis.Elements;
   begin
      case Element_Kind (Element) is
         when A_Clause =>
            case Clause_Kind (Element) is
            when A_Component_Clause =>
               return Clause_Kinds'Image (A_Component_Clause);

            when A_Representation_Clause =>
               case Representation_Clause_Kind (Element) is
               when A_Record_Representation_Clause =>
                  return Representation_Clause_Kinds'Image (A_Record_Representation_Clause);

               when An_At_Clause =>
                  return Representation_Clause_Kinds'Image (An_At_Clause);

               when An_Attribute_Definition_Clause =>
                  return Representation_Clause_Kinds'Image (An_Attribute_Definition_Clause);

               when An_Enumeration_Representation_Clause =>
                  return Representation_Clause_Kinds'Image (An_Enumeration_Representation_Clause);

               when Not_A_Representation_Clause =>
                  --  Nothing to do here
                  null;
               end case;

            when A_Use_All_Type_Clause =>
               return Clause_Kinds'Image (A_Use_All_Type_Clause);

            when A_Use_Package_Clause =>
               return Clause_Kinds'Image (A_Use_Package_Clause);

            when A_Use_Type_Clause =>
               return Clause_Kinds'Image (A_Use_Type_Clause);

            when A_With_Clause =>
               return Clause_Kinds'Image (A_With_Clause);

            when Not_A_Clause =>
               --  Nothing to do here
               null;
            end case;

         when A_Declaration =>
            case Declaration_Kind (Element) is
            when A_Choice_Parameter_Specification =>
               return Declaration_Kinds'Image (A_Choice_Parameter_Specification);

            when A_Component_Declaration =>
               return Declaration_Kinds'Image (A_Component_Declaration);

            when A_Constant_Declaration =>
               return Declaration_Kinds'Image (A_Constant_Declaration);

            when A_Deferred_Constant_Declaration =>
               return Declaration_Kinds'Image (A_Deferred_Constant_Declaration);

            when A_Discriminant_Specification =>
               return Declaration_Kinds'Image (A_Discriminant_Specification);

            when A_Formal_Function_Declaration =>
               case Default_Kind (Element) is
               when A_Box_Default =>
                  return Subprogram_Default_Kinds'Image (A_Box_Default);

               when A_Name_Default =>
                  return Subprogram_Default_Kinds'Image (A_Name_Default);

               when A_Nil_Default =>
                  return Subprogram_Default_Kinds'Image (A_Nil_Default);

               when A_Null_Default =>
                  return Subprogram_Default_Kinds'Image (A_Null_Default);

               when Not_A_Default =>
                  --  Nothing to do here
                  null;
               end case;

            when A_Formal_Incomplete_Type_Declaration =>
               return Declaration_Kinds'Image (A_Formal_Incomplete_Type_Declaration);

            when A_Formal_Object_Declaration =>
               case Mode_Kind (Element) is
               when A_Default_In_Mode =>
                  return Mode_Kinds'Image (A_Default_In_Mode);

               when An_In_Mode =>
                  return Mode_Kinds'Image (An_In_Mode);

               when An_In_Out_Mode =>
                  return Mode_Kinds'Image (An_In_Out_Mode);

               when An_Out_Mode =>
                  return Mode_Kinds'Image (An_Out_Mode);

               when Not_A_Mode =>
                  --  Nothing to do here
                  null;
               end case;

            when A_Formal_Package_Declaration =>
               return Declaration_Kinds'Image (A_Formal_Package_Declaration);

            when A_Formal_Package_Declaration_With_Box =>
               return Declaration_Kinds'Image (A_Formal_Package_Declaration_With_Box);

            when A_Formal_Procedure_Declaration =>
               case Default_Kind (Element) is
               when A_Box_Default =>
                  return Subprogram_Default_Kinds'Image (A_Box_Default);

               when A_Name_Default =>
                  return Subprogram_Default_Kinds'Image (A_Name_Default);

               when A_Nil_Default =>
                  return Subprogram_Default_Kinds'Image (A_Nil_Default);

               when A_Null_Default =>
                  return Subprogram_Default_Kinds'Image (A_Null_Default);

               when Not_A_Default =>
                  --  Nothing to do here
                  null;
               end case;

            when A_Formal_Type_Declaration =>
               return Declaration_Kinds'Image (A_Formal_Type_Declaration);

            when A_Function_Body_Declaration =>
               return Declaration_Kinds'Image (A_Function_Body_Declaration);

            when A_Function_Body_Stub =>
               return Declaration_Kinds'Image (A_Function_Body_Stub);

            when A_Function_Declaration =>
               return Declaration_Kinds'Image (A_Function_Declaration);

            when A_Function_Instantiation =>
               return Declaration_Kinds'Image (A_Function_Instantiation);

            when A_Function_Renaming_Declaration =>
               return Declaration_Kinds'Image (A_Function_Renaming_Declaration);

            when A_Generalized_Iterator_Specification =>
               return Declaration_Kinds'Image (A_Generalized_Iterator_Specification);

            when A_Generic_Function_Declaration =>
               return Declaration_Kinds'Image (A_Generic_Function_Declaration);

            when A_Generic_Function_Renaming_Declaration =>
               return Declaration_Kinds'Image (A_Generic_Function_Renaming_Declaration);

            when A_Generic_Package_Declaration =>
               return Declaration_Kinds'Image (A_Generic_Package_Declaration);

            when A_Generic_Package_Renaming_Declaration =>
               return Declaration_Kinds'Image (A_Generic_Package_Renaming_Declaration);

            when A_Generic_Procedure_Declaration =>
               return Declaration_Kinds'Image (A_Generic_Procedure_Declaration);

            when A_Generic_Procedure_Renaming_Declaration =>
               return Declaration_Kinds'Image (A_Generic_Procedure_Renaming_Declaration);

            when A_Loop_Parameter_Specification =>
               return Declaration_Kinds'Image (A_Loop_Parameter_Specification);

            when A_Null_Procedure_Declaration =>
               return Declaration_Kinds'Image (A_Null_Procedure_Declaration);

            when A_Package_Body_Declaration =>
               return Declaration_Kinds'Image (A_Package_Body_Declaration);

            when A_Package_Body_Stub =>
               return Declaration_Kinds'Image (A_Package_Body_Stub);

            when A_Package_Declaration =>
               return Declaration_Kinds'Image (A_Package_Declaration);

            when A_Package_Instantiation =>
               return Declaration_Kinds'Image (A_Package_Instantiation);

            when A_Package_Renaming_Declaration =>
               return Declaration_Kinds'Image (A_Package_Renaming_Declaration);

            when A_Parameter_Specification =>
               case Mode_Kind (Element) is
               when A_Default_In_Mode =>
                  return Mode_Kinds'Image (A_Default_In_Mode);

               when An_In_Mode =>
                  return Mode_Kinds'Image (An_In_Mode);

               when An_In_Out_Mode =>
                  return Mode_Kinds'Image (An_In_Out_Mode);

               when An_Out_Mode =>
                  return Mode_Kinds'Image (An_Out_Mode);

               when Not_A_Mode =>
                  --  Nothing to do here
                  null;
               end case;

            when A_Private_Extension_Declaration =>
               return Declaration_Kinds'Image (A_Private_Extension_Declaration);

            when A_Private_Type_Declaration =>
               return Declaration_Kinds'Image (A_Private_Type_Declaration);

            when A_Procedure_Body_Declaration =>
               return Declaration_Kinds'Image (A_Procedure_Body_Declaration);

            when A_Procedure_Body_Stub =>
               return Declaration_Kinds'Image (A_Procedure_Body_Stub);

            when A_Procedure_Declaration =>
               return Declaration_Kinds'Image (A_Procedure_Declaration);

            when A_Procedure_Instantiation =>
               return Declaration_Kinds'Image (A_Procedure_Instantiation);

            when A_Procedure_Renaming_Declaration =>
               return Declaration_Kinds'Image (A_Procedure_Renaming_Declaration);

            when A_Protected_Body_Declaration =>
               return Declaration_Kinds'Image (A_Protected_Body_Declaration);

            when A_Protected_Body_Stub =>
               return Declaration_Kinds'Image (A_Protected_Body_Stub);

            when A_Protected_Type_Declaration =>
               return Declaration_Kinds'Image (A_Protected_Type_Declaration);

            when A_Real_Number_Declaration =>
               return Declaration_Kinds'Image (A_Real_Number_Declaration);

            when A_Return_Constant_Specification =>
               return Declaration_Kinds'Image (A_Return_Constant_Specification);

            when A_Return_Variable_Specification =>
               return Declaration_Kinds'Image (A_Return_Variable_Specification);

            when A_Single_Protected_Declaration =>
               return Declaration_Kinds'Image (A_Single_Protected_Declaration);

            when A_Single_Task_Declaration =>
               return Declaration_Kinds'Image (A_Single_Task_Declaration);

            when A_Subtype_Declaration =>
               return Declaration_Kinds'Image (A_Subtype_Declaration);

            when A_Tagged_Incomplete_Type_Declaration =>
               return Declaration_Kinds'Image (A_Tagged_Incomplete_Type_Declaration);

            when A_Task_Body_Declaration =>
               return Declaration_Kinds'Image (A_Task_Body_Declaration);

            when A_Task_Body_Stub =>
               return Declaration_Kinds'Image (A_Task_Body_Stub);

            when A_Task_Type_Declaration =>
               return Declaration_Kinds'Image (A_Task_Type_Declaration);

            when A_Variable_Declaration =>
               return Declaration_Kinds'Image (A_Variable_Declaration);

            when An_Element_Iterator_Specification =>
               return Declaration_Kinds'Image (An_Element_Iterator_Specification);

            when An_Entry_Body_Declaration =>
               return Declaration_Kinds'Image (An_Entry_Body_Declaration);

            when An_Entry_Declaration =>
               return Declaration_Kinds'Image (An_Entry_Declaration);

            when An_Entry_Index_Specification =>
               return Declaration_Kinds'Image (An_Entry_Index_Specification);

            when An_Enumeration_Literal_Specification =>
               return Declaration_Kinds'Image (An_Enumeration_Literal_Specification);

            when An_Exception_Declaration =>
               return Declaration_Kinds'Image (An_Exception_Declaration);

            when An_Exception_Renaming_Declaration =>
               return Declaration_Kinds'Image (An_Exception_Renaming_Declaration);

            when An_Expression_Function_Declaration =>
               return Declaration_Kinds'Image (An_Expression_Function_Declaration);

            when An_Incomplete_Type_Declaration =>
               return Declaration_Kinds'Image (An_Incomplete_Type_Declaration);

            when An_Integer_Number_Declaration =>
               return Declaration_Kinds'Image (An_Integer_Number_Declaration);

            when An_Object_Renaming_Declaration =>
               return Declaration_Kinds'Image (An_Object_Renaming_Declaration);

            when An_Ordinary_Type_Declaration =>
               return Declaration_Kinds'Image (An_Ordinary_Type_Declaration);

            when Not_A_Declaration =>
               --  Nothing to do here
               null;
            end case;

         when A_Defining_Name =>
            case Defining_Name_Kind (Element) is
            when A_Defining_Character_Literal =>
               return Defining_Name_Kinds'Image (A_Defining_Character_Literal);

            when A_Defining_Enumeration_Literal =>
               return Defining_Name_Kinds'Image (A_Defining_Enumeration_Literal);

            when A_Defining_Expanded_Name =>
               return Defining_Name_Kinds'Image (A_Defining_Expanded_Name);

            when A_Defining_Identifier =>
               return Defining_Name_Kinds'Image (A_Defining_Identifier);

            when A_Defining_Operator_Symbol =>
               case Operator_Kind (Element) is
               when A_Concatenate_Operator =>
                  return Operator_Kinds'Image (A_Concatenate_Operator);

               when A_Divide_Operator =>
                  return Operator_Kinds'Image (A_Divide_Operator);

               when A_Greater_Than_Operator =>
                  return Operator_Kinds'Image (A_Greater_Than_Operator);

               when A_Greater_Than_Or_Equal_Operator =>
                  return Operator_Kinds'Image (A_Greater_Than_Or_Equal_Operator);

               when A_Less_Than_Operator =>
                  return Operator_Kinds'Image (A_Less_Than_Operator);

               when A_Less_Than_Or_Equal_Operator =>
                  return Operator_Kinds'Image (A_Less_Than_Or_Equal_Operator);

               when A_Minus_Operator =>
                  return Operator_Kinds'Image (A_Minus_Operator);

               when A_Mod_Operator =>
                  return Operator_Kinds'Image (A_Mod_Operator);

               when A_Multiply_Operator =>
                  return Operator_Kinds'Image (A_Multiply_Operator);

               when A_Not_Equal_Operator =>
                  return Operator_Kinds'Image (A_Not_Equal_Operator);

               when A_Not_Operator =>
                  return Operator_Kinds'Image (A_Not_Operator);

               when A_Plus_Operator =>
                  return Operator_Kinds'Image (A_Plus_Operator);

               when A_Rem_Operator =>
                  return Operator_Kinds'Image (A_Rem_Operator);

               when A_Unary_Minus_Operator =>
                  return Operator_Kinds'Image (A_Unary_Minus_Operator);

               when A_Unary_Plus_Operator =>
                  return Operator_Kinds'Image (A_Unary_Plus_Operator);

               when An_Abs_Operator =>
                  return Operator_Kinds'Image (An_Abs_Operator);

               when An_And_Operator =>
                  return Operator_Kinds'Image (An_And_Operator);

               when An_Equal_Operator =>
                  return Operator_Kinds'Image (An_Equal_Operator);

               when An_Exponentiate_Operator =>
                  return Operator_Kinds'Image (An_Exponentiate_Operator);

               when An_Or_Operator =>
                  return Operator_Kinds'Image (An_Or_Operator);

               when An_Xor_Operator =>
                  return Operator_Kinds'Image (An_Xor_Operator);

               when Not_An_Operator =>
                  --  Nothing to do here
                  null;
               end case;

            when Not_A_Defining_Name =>
               --  Nothing to do here
               null;
            end case;

         when A_Definition =>
            case Definition_Kind (Element) is
            when A_Component_Definition =>
               return Definition_Kinds'Image (A_Component_Definition);

            when A_Constraint =>
               case Constraint_Kind (Element) is
               when A_Delta_Constraint =>
                  return Constraint_Kinds'Image (A_Delta_Constraint);

               when A_Digits_Constraint =>
                  return Constraint_Kinds'Image (A_Digits_Constraint);

               when A_Discriminant_Constraint =>
                  return Constraint_Kinds'Image (A_Discriminant_Constraint);

               when A_Range_Attribute_Reference =>
                  return Constraint_Kinds'Image (A_Range_Attribute_Reference);

               when A_Simple_Expression_Range =>
                  return Constraint_Kinds'Image (A_Simple_Expression_Range);

               when An_Index_Constraint =>
                  return Constraint_Kinds'Image (An_Index_Constraint);

               when Not_A_Constraint =>
                  --  Nothing to do here
                  null;
               end case;

            when A_Discrete_Range =>
               case Discrete_Range_Kind (Element) is
               when A_Discrete_Range_Attribute_Reference =>
                  return Discrete_Range_Kinds'Image (A_Discrete_Range_Attribute_Reference);

               when A_Discrete_Simple_Expression_Range =>
                  return Discrete_Range_Kinds'Image (A_Discrete_Simple_Expression_Range);

               when A_Discrete_Subtype_Indication =>
                  return Discrete_Range_Kinds'Image (A_Discrete_Subtype_Indication);

               when Not_A_Discrete_Range =>
                  --  Nothing to do here
                  null;
               end case;

            when A_Discrete_Subtype_Definition =>
               case Discrete_Range_Kind (Element) is
               when A_Discrete_Range_Attribute_Reference =>
                  return Discrete_Range_Kinds'Image (A_Discrete_Range_Attribute_Reference);

               when A_Discrete_Simple_Expression_Range =>
                  return Discrete_Range_Kinds'Image (A_Discrete_Simple_Expression_Range);

               when A_Discrete_Subtype_Indication =>
                  return Discrete_Range_Kinds'Image (A_Discrete_Subtype_Indication);

               when Not_A_Discrete_Range =>
                  --  Nothing to do here
                  null;
               end case;

            when A_Formal_Type_Definition =>
               case Formal_Type_Kind (Element) is
               when A_Formal_Access_Type_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Access_Type_Definition);

               when A_Formal_Constrained_Array_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Constrained_Array_Definition);

               when A_Formal_Decimal_Fixed_Point_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Decimal_Fixed_Point_Definition);

               when A_Formal_Derived_Type_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Derived_Type_Definition);

               when A_Formal_Discrete_Type_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Discrete_Type_Definition);

               when A_Formal_Floating_Point_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Floating_Point_Definition);

               when A_Formal_Interface_Type_Definition =>
                  case Interface_Kind (Element) is
                     when A_Limited_Interface =>
                        return Interface_Kinds'Image (A_Limited_Interface);

                     when A_Protected_Interface =>
                        return Interface_Kinds'Image (A_Protected_Interface);

                     when A_Synchronized_Interface =>
                        return Interface_Kinds'Image (A_Synchronized_Interface);

                     when A_Task_Interface =>
                        return Interface_Kinds'Image (A_Task_Interface);

                     when An_Ordinary_Interface =>
                        return Interface_Kinds'Image (An_Ordinary_Interface);

                     when Not_An_Interface =>
                        return Interface_Kinds'Image (Not_An_Interface);
                  end case;

               when A_Formal_Modular_Type_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Modular_Type_Definition);

               when A_Formal_Ordinary_Fixed_Point_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Ordinary_Fixed_Point_Definition);

               when A_Formal_Private_Type_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Private_Type_Definition);

               when A_Formal_Signed_Integer_Type_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Signed_Integer_Type_Definition);

               when A_Formal_Tagged_Private_Type_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Tagged_Private_Type_Definition);

               when A_Formal_Unconstrained_Array_Definition =>
                  return Formal_Type_Kinds'Image (A_Formal_Unconstrained_Array_Definition);

               when Not_A_Formal_Type_Definition =>
                  --  Nothing to do here
                  null;
               end case;

            when A_Known_Discriminant_Part =>
               return Definition_Kinds'Image (A_Known_Discriminant_Part);

            when A_Null_Component =>
               return Definition_Kinds'Image (A_Null_Component);

            when A_Null_Record_Definition =>
               return Definition_Kinds'Image (A_Null_Record_Definition);

            when A_Private_Extension_Definition =>
               return Definition_Kinds'Image (A_Private_Extension_Definition);

            when A_Private_Type_Definition =>
               return Definition_Kinds'Image (A_Private_Type_Definition);

            when A_Protected_Definition =>
               return Definition_Kinds'Image (A_Protected_Definition);

            when A_Record_Definition =>
               return Definition_Kinds'Image (A_Record_Definition);

            when A_Subtype_Indication =>
               return Definition_Kinds'Image (A_Subtype_Indication);

            when A_Tagged_Private_Type_Definition =>
               return Definition_Kinds'Image (A_Tagged_Private_Type_Definition);

            when A_Task_Definition =>
               return Definition_Kinds'Image (A_Task_Definition);

            when A_Type_Definition =>
               case Type_Kind (Element) is
               when A_Constrained_Array_Definition =>
                  return Type_Kinds'Image (A_Constrained_Array_Definition);

               when A_Decimal_Fixed_Point_Definition =>
                  return Type_Kinds'Image (A_Decimal_Fixed_Point_Definition);

               when A_Derived_Record_Extension_Definition =>
                  return Type_Kinds'Image (A_Derived_Record_Extension_Definition);

               when A_Derived_Type_Definition =>
                  return Type_Kinds'Image (A_Derived_Type_Definition);

               when A_Floating_Point_Definition =>
                  return Type_Kinds'Image (A_Floating_Point_Definition);

               when A_Modular_Type_Definition =>
                  return Type_Kinds'Image (A_Modular_Type_Definition);

               when A_Record_Type_Definition =>
                  return Type_Kinds'Image (A_Record_Type_Definition);

               when A_Root_Type_Definition =>
                  case Root_Type_Kind (Element) is
                     when A_Root_Integer_Definition =>
                        return Root_Type_Kinds'Image (A_Root_Integer_Definition);

                     when A_Root_Real_Definition =>
                        return Root_Type_Kinds'Image (A_Root_Real_Definition);

                     when A_Universal_Fixed_Definition =>
                        return Root_Type_Kinds'Image (A_Universal_Fixed_Definition);

                     when A_Universal_Integer_Definition =>
                        return Root_Type_Kinds'Image (A_Universal_Integer_Definition);

                     when A_Universal_Real_Definition =>
                        return Root_Type_Kinds'Image (A_Universal_Real_Definition);

                     when Not_A_Root_Type_Definition =>
                        --  Nothing to do here
                        null;
                  end case;

               when A_Signed_Integer_Type_Definition =>
                  return Type_Kinds'Image (A_Signed_Integer_Type_Definition);

               when A_Tagged_Record_Type_Definition =>
                  return Type_Kinds'Image (A_Tagged_Record_Type_Definition);

               when An_Access_Type_Definition =>
                  case Access_Type_Kind (Element) is
                     when A_Pool_Specific_Access_To_Variable =>
                        return Access_Type_Kinds'Image (A_Pool_Specific_Access_To_Variable);

                     when An_Access_To_Constant =>
                        return Access_Type_Kinds'Image (An_Access_To_Constant);

                     when An_Access_To_Function =>
                        return Access_Type_Kinds'Image (An_Access_To_Function);

                     when An_Access_To_Procedure =>
                        return Access_Type_Kinds'Image (An_Access_To_Procedure);

                     when An_Access_To_Protected_Function =>
                        return Access_Type_Kinds'Image (An_Access_To_Protected_Function);

                     when An_Access_To_Protected_Procedure =>
                        return Access_Type_Kinds'Image (An_Access_To_Protected_Procedure);

                     when An_Access_To_Variable =>
                        return Access_Type_Kinds'Image (An_Access_To_Variable);

                     when Not_An_Access_Type_Definition =>
                        --  Nothing to do here
                        null;
                  end case;

               when An_Enumeration_Type_Definition =>
                  return Type_Kinds'Image (An_Enumeration_Type_Definition);

               when An_Interface_Type_Definition =>
                  case Interface_Kind (Element) is
                     when A_Limited_Interface =>
                        return Interface_Kinds'Image (A_Limited_Interface);

                     when A_Protected_Interface =>
                        return Interface_Kinds'Image (A_Protected_Interface);

                     when A_Synchronized_Interface =>
                        return Interface_Kinds'Image (A_Synchronized_Interface);

                     when A_Task_Interface =>
                        return Interface_Kinds'Image (A_Task_Interface);

                     when An_Ordinary_Interface =>
                        return Interface_Kinds'Image (An_Ordinary_Interface);

                     when Not_An_Interface =>
                        --  Nothing to do here
                        null;
                  end case;

               when An_Ordinary_Fixed_Point_Definition =>
                  return Type_Kinds'Image (An_Ordinary_Fixed_Point_Definition);

               when An_Unconstrained_Array_Definition =>
                  return Type_Kinds'Image (An_Unconstrained_Array_Definition);

               when Not_A_Type_Definition =>
                  --  Nothing to do here
                  null;
               end case;
               return Definition_Kinds'Image (A_Type_Definition);

            when A_Variant =>
               return Definition_Kinds'Image (A_Variant);

            when A_Variant_Part =>
               return Definition_Kinds'Image (A_Variant_Part);

            when An_Access_Definition =>
               case Access_Definition_Kind (Element) is
               when An_Anonymous_Access_To_Constant =>
                  return Access_Definition_Kinds'Image (An_Anonymous_Access_To_Constant);

               when An_Anonymous_Access_To_Function =>
                  return Access_Definition_Kinds'Image (An_Anonymous_Access_To_Function);

               when An_Anonymous_Access_To_Procedure =>
                  return Access_Definition_Kinds'Image (An_Anonymous_Access_To_Procedure);

               when An_Anonymous_Access_To_Protected_Function =>
                  return Access_Definition_Kinds'Image (An_Anonymous_Access_To_Protected_Function);

               when An_Anonymous_Access_To_Protected_Procedure =>
                  return Access_Definition_Kinds'Image (An_Anonymous_Access_To_Protected_Procedure);

               when An_Anonymous_Access_To_Variable =>
                  return Access_Definition_Kinds'Image (An_Anonymous_Access_To_Variable);

               when Not_An_Access_Definition =>
                  --  Nothing to do here
                  null;
               end case;

            when An_Aspect_Specification =>
               return Definition_Kinds'Image (An_Aspect_Specification);

            when An_Others_Choice =>
               return Definition_Kinds'Image (An_Others_Choice);

            when An_Unknown_Discriminant_Part =>
               return Definition_Kinds'Image (An_Unknown_Discriminant_Part);

            when Not_A_Definition =>
               --  Nothing to do here
               null;
            end case;

         when A_Path =>
            case Path_Kind (Element) is
            when A_Case_Expression_Path =>
               return Path_Kinds'Image (A_Case_Expression_Path);

            when A_Case_Path =>
               return Path_Kinds'Image (A_Case_Path);

            when A_Select_Path =>
               return Path_Kinds'Image (A_Select_Path);

            when A_Then_Abort_Path =>
               return Path_Kinds'Image (A_Then_Abort_Path);

            when An_Else_Expression_Path =>
               return Path_Kinds'Image (An_Else_Expression_Path);

            when An_Else_Path =>
               return Path_Kinds'Image (An_Else_Path);

            when An_Elsif_Expression_Path =>
               return Path_Kinds'Image (An_Elsif_Expression_Path);

            when An_Elsif_Path =>
               return Path_Kinds'Image (An_Elsif_Path);

            when An_If_Expression_Path =>
               return Path_Kinds'Image (An_If_Expression_Path);

            when An_If_Path =>
               return Path_Kinds'Image (An_If_Path);

            when An_Or_Path =>
               return Path_Kinds'Image (An_Or_Path);

            when Not_A_Path =>
               --  Nothing to do here
               null;
            end case;

         when A_Pragma =>
            case Pragma_Kind (Element) is
            when A_Controlled_Pragma =>
               return Pragma_Kinds'Image (A_Controlled_Pragma);

            when A_Convention_Pragma =>
               return Pragma_Kinds'Image (A_Convention_Pragma);

            when A_CPU_Pragma =>
               return Pragma_Kinds'Image (A_CPU_Pragma);

            when A_Default_Storage_Pool_Pragma =>
               return Pragma_Kinds'Image (A_Default_Storage_Pool_Pragma);

            when A_Detect_Blocking_Pragma =>
               return Pragma_Kinds'Image (A_Detect_Blocking_Pragma);

            when A_Discard_Names_Pragma =>
               return Pragma_Kinds'Image (A_Discard_Names_Pragma);

            when A_Dispatching_Domain_Pragma =>
               return Pragma_Kinds'Image (A_Dispatching_Domain_Pragma);

            when A_Independent_Components_Pragma =>
               return Pragma_Kinds'Image (A_Independent_Components_Pragma);

            when A_Linker_Options_Pragma =>
               return Pragma_Kinds'Image (A_Linker_Options_Pragma);

            when A_List_Pragma =>
               return Pragma_Kinds'Image (A_List_Pragma);

            when A_Locking_Policy_Pragma =>
               return Pragma_Kinds'Image (A_Locking_Policy_Pragma);

            when A_No_Return_Pragma =>
               return Pragma_Kinds'Image (A_No_Return_Pragma);

            when A_Normalize_Scalars_Pragma =>
               return Pragma_Kinds'Image (A_Normalize_Scalars_Pragma);

            when A_Pack_Pragma =>
               return Pragma_Kinds'Image (A_Pack_Pragma);

            when A_Page_Pragma =>
               return Pragma_Kinds'Image (A_Page_Pragma);

            when A_Partition_Elaboration_Policy_Pragma =>
               return Pragma_Kinds'Image (A_Partition_Elaboration_Policy_Pragma);

            when A_Preelaborable_Initialization_Pragma =>
               return Pragma_Kinds'Image (A_Preelaborable_Initialization_Pragma);

            when A_Preelaborate_Pragma =>
               return Pragma_Kinds'Image (A_Preelaborate_Pragma);

            when A_Priority_Pragma =>
               return Pragma_Kinds'Image (A_Priority_Pragma);

            when A_Priority_Specific_Dispatching_Pragma =>
               return Pragma_Kinds'Image (A_Priority_Specific_Dispatching_Pragma);

            when A_Profile_Pragma =>
               return Pragma_Kinds'Image (A_Profile_Pragma);

            when A_Pure_Pragma =>
               return Pragma_Kinds'Image (A_Pure_Pragma);

            when A_Queuing_Policy_Pragma =>
               return Pragma_Kinds'Image (A_Queuing_Policy_Pragma);

            when A_Relative_Deadline_Pragma =>
               return Pragma_Kinds'Image (A_Relative_Deadline_Pragma);

            when A_Remote_Call_Interface_Pragma =>
               return Pragma_Kinds'Image (A_Remote_Call_Interface_Pragma);

            when A_Remote_Types_Pragma =>
               return Pragma_Kinds'Image (A_Remote_Types_Pragma);

            when A_Restrictions_Pragma =>
               return Pragma_Kinds'Image (A_Restrictions_Pragma);

            when A_Reviewable_Pragma =>
               return Pragma_Kinds'Image (A_Reviewable_Pragma);

            when A_Shared_Passive_Pragma =>
               return Pragma_Kinds'Image (A_Shared_Passive_Pragma);

            when A_Storage_Size_Pragma =>
               return Pragma_Kinds'Image (A_Storage_Size_Pragma);

            when A_Suppress_Pragma =>
               return Pragma_Kinds'Image (A_Suppress_Pragma);

            when A_Task_Dispatching_Policy_Pragma =>
               return Pragma_Kinds'Image (A_Task_Dispatching_Policy_Pragma);

            when A_Volatile_Components_Pragma =>
               return Pragma_Kinds'Image (A_Volatile_Components_Pragma);

            when A_Volatile_Pragma =>
               return Pragma_Kinds'Image (A_Volatile_Pragma);

            when An_All_Calls_Remote_Pragma =>
               return Pragma_Kinds'Image (An_All_Calls_Remote_Pragma);

            when An_Assert_Pragma =>
               return Pragma_Kinds'Image (An_Assert_Pragma);

            when An_Assertion_Policy_Pragma =>
               return Pragma_Kinds'Image (An_Assertion_Policy_Pragma);

            when An_Asynchronous_Pragma =>
               return Pragma_Kinds'Image (An_Asynchronous_Pragma);

            when An_Atomic_Components_Pragma =>
               return Pragma_Kinds'Image (An_Atomic_Components_Pragma);

            when An_Atomic_Pragma =>
               return Pragma_Kinds'Image (An_Atomic_Pragma);

            when An_Attach_Handler_Pragma =>
               return Pragma_Kinds'Image (An_Attach_Handler_Pragma);

            when An_Elaborate_All_Pragma =>
               return Pragma_Kinds'Image (An_Elaborate_All_Pragma);

            when An_Elaborate_Body_Pragma =>
               return Pragma_Kinds'Image (An_Elaborate_Body_Pragma);

            when An_Elaborate_Pragma =>
               return Pragma_Kinds'Image (An_Elaborate_Pragma);

            when An_Export_Pragma =>
               return Pragma_Kinds'Image (An_Export_Pragma);

            when An_Implementation_Defined_Pragma =>
               return Pragma_Kinds'Image (An_Implementation_Defined_Pragma);

            when An_Import_Pragma =>
               return Pragma_Kinds'Image (An_Import_Pragma);

            when An_Independent_Pragma =>
               return Pragma_Kinds'Image (An_Independent_Pragma);

            when An_Inline_Pragma =>
               return Pragma_Kinds'Image (An_Inline_Pragma);

            when An_Inspection_Point_Pragma =>
               return Pragma_Kinds'Image (An_Inspection_Point_Pragma);

            when An_Interrupt_Handler_Pragma =>
               return Pragma_Kinds'Image (An_Interrupt_Handler_Pragma);

            when An_Interrupt_Priority_Pragma =>
               return Pragma_Kinds'Image (An_Interrupt_Priority_Pragma);

            when An_Optimize_Pragma =>
               return Pragma_Kinds'Image (An_Optimize_Pragma);

            when An_Unchecked_Union_Pragma =>
               return Pragma_Kinds'Image (An_Unchecked_Union_Pragma);

            when An_Unknown_Pragma =>
               return Pragma_Kinds'Image (An_Unknown_Pragma);

            when An_Unsuppress_Pragma =>
               return Pragma_Kinds'Image (An_Unsuppress_Pragma);

            when Not_A_Pragma =>
               --  Nothing to do here
               null;
            end case;

         when A_Statement =>
            case Statement_Kind (Element) is
            when A_Block_Statement =>
               return Statement_Kinds'Image (A_Block_Statement);

            when A_Case_Statement =>
               return Statement_Kinds'Image (A_Case_Statement);

            when A_Code_Statement =>
               return Statement_Kinds'Image (A_Code_Statement);

            when A_Conditional_Entry_Call_Statement =>
               return Statement_Kinds'Image (A_Conditional_Entry_Call_Statement);

            when A_Delay_Relative_Statement =>
               return Statement_Kinds'Image (A_Delay_Relative_Statement);

            when A_Delay_Until_Statement =>
               return Statement_Kinds'Image (A_Delay_Until_Statement);

            when A_For_Loop_Statement =>
               return Statement_Kinds'Image (A_For_Loop_Statement);

            when A_Goto_Statement =>
               return Statement_Kinds'Image (A_Goto_Statement);

            when A_Loop_Statement =>
               return Statement_Kinds'Image (A_Loop_Statement);

            when A_Null_Statement =>
               return Statement_Kinds'Image (A_Null_Statement);

            when A_Procedure_Call_Statement =>
               return Statement_Kinds'Image (A_Procedure_Call_Statement);

            when A_Raise_Statement =>
               return Statement_Kinds'Image (A_Raise_Statement);

            when A_Requeue_Statement =>
               return Statement_Kinds'Image (A_Requeue_Statement);

            when A_Requeue_Statement_With_Abort =>
               return Statement_Kinds'Image (A_Requeue_Statement_With_Abort);

            when A_Return_Statement =>
               return Statement_Kinds'Image (A_Return_Statement);

            when A_Selective_Accept_Statement =>
               return Statement_Kinds'Image (A_Selective_Accept_Statement);

            when A_Terminate_Alternative_Statement =>
               return Statement_Kinds'Image (A_Terminate_Alternative_Statement);

            when A_Timed_Entry_Call_Statement =>
               return Statement_Kinds'Image (A_Timed_Entry_Call_Statement);

            when A_While_Loop_Statement =>
               return Statement_Kinds'Image (A_While_Loop_Statement);

            when An_Abort_Statement =>
               return Statement_Kinds'Image (An_Abort_Statement);

            when An_Accept_Statement =>
               return Statement_Kinds'Image (An_Accept_Statement);

            when An_Assignment_Statement =>
               return Statement_Kinds'Image (An_Assignment_Statement);

            when An_Asynchronous_Select_Statement =>
               return Statement_Kinds'Image (An_Asynchronous_Select_Statement);

            when An_Entry_Call_Statement =>
               return Statement_Kinds'Image (An_Entry_Call_Statement);

            when An_Exit_Statement =>
               return Statement_Kinds'Image (An_Exit_Statement);

            when An_Extended_Return_Statement =>
               return Statement_Kinds'Image (An_Extended_Return_Statement);

            when An_If_Statement =>
               return Statement_Kinds'Image (An_If_Statement);

            when Not_A_Statement =>
               --  Nothing to do here
               null;
            end case;

         when An_Association =>
            case Association_Kind (Element) is
            when A_Discriminant_Association =>
               return Association_Kinds'Image (A_Discriminant_Association);

            when A_Generic_Association =>
               return Association_Kinds'Image (A_Generic_Association);

            when A_Parameter_Association =>
               return Association_Kinds'Image (A_Parameter_Association);

            when A_Pragma_Argument_Association =>
               return Association_Kinds'Image (A_Pragma_Argument_Association);

            when A_Record_Component_Association =>
               return Association_Kinds'Image (A_Record_Component_Association);

            when An_Array_Component_Association =>
               return Association_Kinds'Image (An_Array_Component_Association);

            when Not_An_Association =>
               --  Nothing to do here
               null;
            end case;

         when An_Exception_Handler =>
            --  There is no "Exception_Handler_Kind".
            --  TODO
            null;

         when An_Expression =>
            case Expression_Kind (Element) is
            when A_Box_Expression =>
               return Expression_Kinds'Image (A_Box_Expression);

            when A_Case_Expression =>
               return Expression_Kinds'Image (A_Case_Expression);

            when A_Character_Literal =>
               return Expression_Kinds'Image (A_Character_Literal);

            when A_For_All_Quantified_Expression =>
               return Expression_Kinds'Image (A_For_All_Quantified_Expression);

            when A_For_Some_Quantified_Expression =>
               return Expression_Kinds'Image (A_For_Some_Quantified_Expression);

            when A_Function_Call =>
               return Expression_Kinds'Image (A_Function_Call);

            when A_Named_Array_Aggregate =>
               return Expression_Kinds'Image (A_Named_Array_Aggregate);

            when A_Not_In_Membership_Test =>
               return Expression_Kinds'Image (A_Not_In_Membership_Test);

            when A_Null_Literal =>
               return Expression_Kinds'Image (A_Null_Literal);

            when A_Parenthesized_Expression =>
               return Expression_Kinds'Image (A_Parenthesized_Expression);

            when A_Positional_Array_Aggregate =>
               return Expression_Kinds'Image (A_Positional_Array_Aggregate);

            when A_Qualified_Expression =>
               return Expression_Kinds'Image (A_Qualified_Expression);

            when A_Raise_Expression =>
               return Expression_Kinds'Image (A_Raise_Expression);

            when A_Real_Literal =>
               return Expression_Kinds'Image (A_Real_Literal);

            when A_Record_Aggregate =>
               return Expression_Kinds'Image (A_Record_Aggregate);

            when A_Selected_Component =>
               return Expression_Kinds'Image (A_Selected_Component);

            when A_Slice =>
               return Expression_Kinds'Image (A_Slice);

            when A_String_Literal =>
               return Expression_Kinds'Image (A_String_Literal);

            when A_Type_Conversion =>
               return Expression_Kinds'Image (A_Type_Conversion);

            when An_Allocation_From_Qualified_Expression =>
               return Expression_Kinds'Image (An_Allocation_From_Qualified_Expression);

            when An_Allocation_From_Subtype =>
               return Expression_Kinds'Image (An_Allocation_From_Subtype);

            when An_And_Then_Short_Circuit =>
               return Expression_Kinds'Image (An_And_Then_Short_Circuit);

            when An_Attribute_Reference =>
               case Attribute_Kind (Element) is
               when A_Base_Attribute =>
                  return Attribute_Kinds'Image (A_Base_Attribute);

               when A_Bit_Order_Attribute =>
                  return Attribute_Kinds'Image (A_Bit_Order_Attribute);

               when A_Body_Version_Attribute =>
                  return Attribute_Kinds'Image (A_Body_Version_Attribute);

               when A_Callable_Attribute =>
                  return Attribute_Kinds'Image (A_Callable_Attribute);

               when A_Caller_Attribute =>
                  return Attribute_Kinds'Image (A_Caller_Attribute);

               when A_Ceiling_Attribute =>
                  return Attribute_Kinds'Image (A_Ceiling_Attribute);

               when A_Class_Attribute =>
                  return Attribute_Kinds'Image (A_Class_Attribute);

               when A_Component_Size_Attribute =>
                  return Attribute_Kinds'Image (A_Component_Size_Attribute);

               when A_Compose_Attribute =>
                  return Attribute_Kinds'Image (A_Compose_Attribute);

               when A_Constrained_Attribute =>
                  return Attribute_Kinds'Image (A_Constrained_Attribute);

               when A_Copy_Sign_Attribute =>
                  return Attribute_Kinds'Image (A_Copy_Sign_Attribute);

               when A_Count_Attribute =>
                  return Attribute_Kinds'Image (A_Count_Attribute);

               when A_Definite_Attribute =>
                  return Attribute_Kinds'Image (A_Definite_Attribute);

               when A_Delta_Attribute =>
                  return Attribute_Kinds'Image (A_Delta_Attribute);

               when A_Denorm_Attribute =>
                  return Attribute_Kinds'Image (A_Denorm_Attribute);

               when A_Digits_Attribute =>
                  return Attribute_Kinds'Image (A_Digits_Attribute);

               when A_First_Attribute =>
                  return Attribute_Kinds'Image (A_First_Attribute);

               when A_First_Bit_Attribute =>
                  return Attribute_Kinds'Image (A_First_Bit_Attribute);

               when A_Floor_Attribute =>
                  return Attribute_Kinds'Image (A_Floor_Attribute);

               when A_Fore_Attribute =>
                  return Attribute_Kinds'Image (A_Fore_Attribute);

               when A_Fraction_Attribute =>
                  return Attribute_Kinds'Image (A_Fraction_Attribute);

               when A_Last_Attribute =>
                  return Attribute_Kinds'Image (A_Last_Attribute);

               when A_Last_Bit_Attribute =>
                  return Attribute_Kinds'Image (A_Last_Bit_Attribute);

               when A_Leading_Part_Attribute =>
                  return Attribute_Kinds'Image (A_Leading_Part_Attribute);

               when A_Length_Attribute =>
                  return Attribute_Kinds'Image (A_Length_Attribute);

               when A_Machine_Attribute =>
                  return Attribute_Kinds'Image (A_Machine_Attribute);

               when A_Machine_Emax_Attribute =>
                  return Attribute_Kinds'Image (A_Machine_Emax_Attribute);

               when A_Machine_Emin_Attribute =>
                  return Attribute_Kinds'Image (A_Machine_Emin_Attribute);

               when A_Machine_Mantissa_Attribute =>
                  return Attribute_Kinds'Image (A_Machine_Mantissa_Attribute);

               when A_Machine_Overflows_Attribute =>
                  return Attribute_Kinds'Image (A_Machine_Overflows_Attribute);

               when A_Machine_Radix_Attribute =>
                  return Attribute_Kinds'Image (A_Machine_Radix_Attribute);

               when A_Machine_Rounding_Attribute =>
                  return Attribute_Kinds'Image (A_Machine_Rounding_Attribute);

               when A_Machine_Rounds_Attribute =>
                  return Attribute_Kinds'Image (A_Machine_Rounds_Attribute);

               when A_Max_Alignment_For_Allocation_Attribute =>
                  return Attribute_Kinds'Image (A_Max_Alignment_For_Allocation_Attribute);

               when A_Max_Attribute =>
                  return Attribute_Kinds'Image (A_Max_Attribute);

               when A_Max_Size_In_Storage_Elements_Attribute =>
                  return Attribute_Kinds'Image (A_Max_Size_In_Storage_Elements_Attribute);

               when A_Min_Attribute =>
                  return Attribute_Kinds'Image (A_Min_Attribute);

               when A_Mod_Attribute =>
                  return Attribute_Kinds'Image (A_Mod_Attribute);

               when A_Model_Attribute =>
                  return Attribute_Kinds'Image (A_Model_Attribute);

               when A_Model_Emin_Attribute =>
                  return Attribute_Kinds'Image (A_Model_Emin_Attribute);

               when A_Model_Epsilon_Attribute =>
                  return Attribute_Kinds'Image (A_Model_Epsilon_Attribute);

               when A_Model_Mantissa_Attribute =>
                  return Attribute_Kinds'Image (A_Model_Mantissa_Attribute);

               when A_Model_Small_Attribute =>
                  return Attribute_Kinds'Image (A_Model_Small_Attribute);

               when A_Modulus_Attribute =>
                  return Attribute_Kinds'Image (A_Modulus_Attribute);

               when A_Partition_ID_Attribute =>
                  return Attribute_Kinds'Image (A_Partition_ID_Attribute);

               when A_Pos_Attribute =>
                  return Attribute_Kinds'Image (A_Pos_Attribute);

               when A_Position_Attribute =>
                  return Attribute_Kinds'Image (A_Position_Attribute);

               when A_Pred_Attribute =>
                  return Attribute_Kinds'Image (A_Pred_Attribute);

               when A_Priority_Attribute =>
                  return Attribute_Kinds'Image (A_Priority_Attribute);

               when A_Range_Attribute =>
                  return Attribute_Kinds'Image (A_Range_Attribute);

               when A_Read_Attribute =>
                  return Attribute_Kinds'Image (A_Read_Attribute);

               when A_Remainder_Attribute =>
                  return Attribute_Kinds'Image (A_Remainder_Attribute);

               when A_Round_Attribute =>
                  return Attribute_Kinds'Image (A_Round_Attribute);

               when A_Rounding_Attribute =>
                  return Attribute_Kinds'Image (A_Rounding_Attribute);

               when A_Safe_First_Attribute =>
                  return Attribute_Kinds'Image (A_Safe_First_Attribute);

               when A_Safe_Last_Attribute =>
                  return Attribute_Kinds'Image (A_Safe_Last_Attribute);

               when A_Scale_Attribute =>
                  return Attribute_Kinds'Image (A_Scale_Attribute);

               when A_Scaling_Attribute =>
                  return Attribute_Kinds'Image (A_Scaling_Attribute);

               when A_Signed_Zeros_Attribute =>
                  return Attribute_Kinds'Image (A_Signed_Zeros_Attribute);

               when A_Size_Attribute =>
                  return Attribute_Kinds'Image (A_Size_Attribute);

               when A_Small_Attribute =>
                  return Attribute_Kinds'Image (A_Small_Attribute);

               when A_Storage_Pool_Attribute =>
                  return Attribute_Kinds'Image (A_Storage_Pool_Attribute);

               when A_Storage_Size_Attribute =>
                  return Attribute_Kinds'Image (A_Storage_Size_Attribute);

               when A_Stream_Size_Attribute =>
                  return Attribute_Kinds'Image (A_Stream_Size_Attribute);

               when A_Succ_Attribute =>
                  return Attribute_Kinds'Image (A_Succ_Attribute);

               when A_Tag_Attribute =>
                  return Attribute_Kinds'Image (A_Tag_Attribute);

               when A_Terminated_Attribute =>
                  return Attribute_Kinds'Image (A_Terminated_Attribute);

               when A_Truncation_Attribute =>
                  return Attribute_Kinds'Image (A_Truncation_Attribute);

               when A_Val_Attribute =>
                  return Attribute_Kinds'Image (A_Val_Attribute);

               when A_Valid_Attribute =>
                  return Attribute_Kinds'Image (A_Valid_Attribute);

               when A_Value_Attribute =>
                  return Attribute_Kinds'Image (A_Value_Attribute);

               when A_Version_Attribute =>
                  return Attribute_Kinds'Image (A_Version_Attribute);

               when A_Wide_Image_Attribute =>
                  return Attribute_Kinds'Image (A_Wide_Image_Attribute);

               when A_Wide_Value_Attribute =>
                  return Attribute_Kinds'Image (A_Wide_Value_Attribute);

               when A_Wide_Wide_Image_Attribute =>
                  return Attribute_Kinds'Image (A_Wide_Wide_Image_Attribute);

               when A_Wide_Wide_Value_Attribute =>
                  return Attribute_Kinds'Image (A_Wide_Wide_Value_Attribute);

               when A_Wide_Wide_Width_Attribute =>
                  return Attribute_Kinds'Image (A_Wide_Wide_Width_Attribute);

               when A_Wide_Width_Attribute =>
                  return Attribute_Kinds'Image (A_Wide_Width_Attribute);

               when A_Width_Attribute =>
                  return Attribute_Kinds'Image (A_Width_Attribute);

               when A_Write_Attribute =>
                  return Attribute_Kinds'Image (A_Write_Attribute);

               when An_Access_Attribute =>
                  return Attribute_Kinds'Image (An_Access_Attribute);

               when An_Address_Attribute =>
                  return Attribute_Kinds'Image (An_Address_Attribute);

               when An_Adjacent_Attribute =>
                  return Attribute_Kinds'Image (An_Adjacent_Attribute);

               when An_Aft_Attribute =>
                  return Attribute_Kinds'Image (An_Aft_Attribute);

               when An_Alignment_Attribute =>
                  return Attribute_Kinds'Image (An_Alignment_Attribute);

               when An_Exponent_Attribute =>
                  return Attribute_Kinds'Image (An_Exponent_Attribute);

               when An_External_Tag_Attribute =>
                  return Attribute_Kinds'Image (An_External_Tag_Attribute);

               when An_Identity_Attribute =>
                  return Attribute_Kinds'Image (An_Identity_Attribute);

               when An_Image_Attribute =>
                  return Attribute_Kinds'Image (An_Image_Attribute);

               when An_Implementation_Defined_Attribute =>
                  return Attribute_Kinds'Image (An_Implementation_Defined_Attribute);

               when An_Input_Attribute =>
                  return Attribute_Kinds'Image (An_Input_Attribute);

               when An_Output_Attribute =>
                  return Attribute_Kinds'Image (An_Output_Attribute);

               when An_Overlaps_Storage_Attribute =>
                  return Attribute_Kinds'Image (An_Overlaps_Storage_Attribute);

               when An_Unbiased_Rounding_Attribute =>
                  return Attribute_Kinds'Image (An_Unbiased_Rounding_Attribute);

               when An_Unchecked_Access_Attribute =>
                  return Attribute_Kinds'Image (An_Unchecked_Access_Attribute);

               when An_Unknown_Attribute =>
                  return Attribute_Kinds'Image (An_Unknown_Attribute);

               when Not_An_Attribute =>
                  --  Nothing to do here
                  null;
               end case;

            when An_Enumeration_Literal =>
               return Expression_Kinds'Image (An_Enumeration_Literal);

            when An_Explicit_Dereference =>
               return Expression_Kinds'Image (An_Explicit_Dereference);

            when An_Extension_Aggregate =>
               return Expression_Kinds'Image (An_Extension_Aggregate);

            when An_Identifier =>
               return Expression_Kinds'Image (An_Identifier);

            when An_If_Expression =>
               return Expression_Kinds'Image (An_If_Expression);

            when An_In_Membership_Test =>
               return Expression_Kinds'Image (An_In_Membership_Test);

            when An_Indexed_Component =>
               return Expression_Kinds'Image (An_Indexed_Component);

            when An_Integer_Literal =>
               return Expression_Kinds'Image (An_Integer_Literal);

            when An_Operator_Symbol =>
               case Operator_Kind (Element) is
               when A_Concatenate_Operator =>
                  return Operator_Kinds'Image (A_Concatenate_Operator);

               when A_Divide_Operator =>
                  return Operator_Kinds'Image (A_Divide_Operator);

               when A_Greater_Than_Operator =>
                  return Operator_Kinds'Image (A_Greater_Than_Operator);

               when A_Greater_Than_Or_Equal_Operator =>
                  return Operator_Kinds'Image (A_Greater_Than_Or_Equal_Operator);

               when A_Less_Than_Operator =>
                  return Operator_Kinds'Image (A_Less_Than_Operator);

               when A_Less_Than_Or_Equal_Operator =>
                  return Operator_Kinds'Image (A_Less_Than_Or_Equal_Operator);

               when A_Minus_Operator =>
                  return Operator_Kinds'Image (A_Minus_Operator);

               when A_Mod_Operator =>
                  return Operator_Kinds'Image (A_Mod_Operator);

               when A_Multiply_Operator =>
                  return Operator_Kinds'Image (A_Multiply_Operator);

               when A_Not_Equal_Operator =>
                  return Operator_Kinds'Image (A_Not_Equal_Operator);

               when A_Not_Operator =>
                  return Operator_Kinds'Image (A_Not_Operator);

               when A_Plus_Operator =>
                  return Operator_Kinds'Image (A_Plus_Operator);

               when A_Rem_Operator =>
                  return Operator_Kinds'Image (A_Rem_Operator);

               when A_Unary_Minus_Operator =>
                  return Operator_Kinds'Image (A_Unary_Minus_Operator);

               when A_Unary_Plus_Operator =>
                  return Operator_Kinds'Image (A_Unary_Plus_Operator);

               when An_Abs_Operator =>
                  return Operator_Kinds'Image (An_Abs_Operator);

               when An_And_Operator =>
                  return Operator_Kinds'Image (An_And_Operator);

               when An_Equal_Operator =>
                  return Operator_Kinds'Image (An_Equal_Operator);

               when An_Exponentiate_Operator =>
                  return Operator_Kinds'Image (An_Exponentiate_Operator);

               when An_Or_Operator =>
                  return Operator_Kinds'Image (An_Or_Operator);

               when An_Xor_Operator =>
                  return Operator_Kinds'Image (An_Xor_Operator);

               when Not_An_Operator =>
                  --  Nothing to do here
                  null;
               end case;

            when An_Or_Else_Short_Circuit =>
               return Expression_Kinds'Image (An_Or_Else_Short_Circuit);

            when A_Target_Name =>
               return Expression_Kinds'Image (A_Target_Name);

            when Not_An_Expression =>
               --  Nothing to do here
               null;
            end case;

         when Not_An_Element =>
            --  Nothing to do here
            null;
      end case;

      return Element_Kinds'Image (Element_Kind (Element));
   end Get_Kind_Image;

   ------------------
   -- Add_Property --
   ------------------

   procedure Add_Property (Arr : out GNATCOLL.JSON.JSON_Array; Key : String; Value : Boolean) is
      use GNATCOLL.JSON;
      use Export_Utils;

      Property : constant JSON_Value := Create_Object;
   begin
      Property.Set_Field ("key", Key);
      Property.Set_Field ("value", Value);
      Property.Set_Field ("_kind", Props_Kind'Image (PK_Boolean));
      Append (Arr, Property);
   end Add_Property;

   ------------------
   -- Add_Property --
   ------------------

   procedure Add_Property (Arr : out GNATCOLL.JSON.JSON_Array; Key : String; Value : Integer) is
      use GNATCOLL.JSON;
      use Export_Utils;

      Property : constant JSON_Value := Create_Object;
   begin
      Property.Set_Field ("key", Key);
      Property.Set_Field ("value", Value);
      Property.Set_Field ("_kind", Props_Kind'Image (PK_Integer));
      Append (Arr, Property);
   end Add_Property;

   -------------------------------
   -- Put_Parameter_Association --
   -------------------------------

   function Put_Parameter_Association (Elt : Parameter_Association; Shift : Wide_String := "") return Wide_String is
      Result : Unbounded_Wide_String := Null_Unbounded_Wide_String;
      LF : constant Wide_String := Ada.Characters.Handling.To_Wide_String (ASCII.LF & "");
      Internal_Shift : constant Wide_String := "  ";
   begin
      Result := Result & Shift & "Parameter_Association:" & LF;
      Result := Result & Shift & Internal_Shift & "Association: "
        & Ada.Characters.Handling.To_Wide_String (Elt.Association.Write) & LF;
      Result := Result & Shift & Internal_Shift & "Corresponding_Actual_Expression: "
        & Ada.Characters.Handling.To_Wide_String (Elt.Corresponding_Actual_Expression.Write) & LF;

      return To_Wide_String (Result);
   end Put_Parameter_Association;

   -------------------------
   -- Put_Parameters_Info --
   -------------------------

   function Put_Parameters_Info (Elt : Parameters_Info) return Wide_String is
      Result : Unbounded_Wide_String := Null_Unbounded_Wide_String;
      LF : constant Wide_String := Ada.Characters.Handling.To_Wide_String (ASCII.LF & "");
      Internal_Shift : constant Wide_String := "  ";
   begin
      Result := Result & "Parameters_Info:" & LF;
      Result := Result & Internal_Shift & "Association:" & LF
        & Put_Parameter_Association (Elt.Association, Internal_Shift);
      Result := Result & Internal_Shift & "Elements:" & Ada.Containers.Count_Type'Wide_Image (Elt.Elements.Length) & LF;
      Result := Result & Internal_Shift & "Current_Primitive_Index:"
        & Positive'Wide_Image (Elt.Current_Primitive_Index) & LF;
      return To_Wide_String (Result);
   end Put_Parameters_Info;

   --  Create a unique ID based on location of the element.
   --  @param Element A JSON Object that should contains:
   --    - location: an object that contains "filename", "line" and "column"
   --    - kind: kind of the node at the specific location
   --  @return Return an ID in any cases
   function Get_Node_Id (Element : GNATCOLL.JSON.JSON_Value) return String is
      use Ada.Strings.Unbounded;
      use GNATCOLL.JSON;

      function JSON_To_String (Element : JSON_Value) return String is
      begin
         case Kind (Element) is
            when JSON_Null_Type =>
               null;
            when JSON_String_Type =>
               declare
                  R : constant String := Element.Get;
               begin
                  return R;
               end;
            when JSON_Boolean_Type | JSON_Int_Type | JSON_Float_Type | JSON_Array_Type | JSON_Object_Type =>
               return Element.Write;
         end case;

         return "";
      end JSON_To_String;

      Name : Unbounded_String := Null_Unbounded_String;
   begin -- Get_Node_Id
      if Has_Field (Element, "location") then
         declare
            filename : constant String     := JSON_To_String (Element.Get ("location").Get ("filename"));
            line     : constant String     := JSON_To_String (Element.Get ("location").Get ("line"));
            column   : constant String     := JSON_To_String (Element.Get ("location").Get ("column"));
         begin
            Name :=
              To_Unbounded_String (filename) & ":" & To_Unbounded_String (line) & ":" & To_Unbounded_String (column);
         end;
      end if;

      if Has_Field (Element, "kind") then
         declare
            Kind : constant String := Element.Get ("kind");
         begin
            Name := Name & ":" & To_Unbounded_String (Kind);
         end;
      elsif Has_Field (Element, "kinds") then
         declare
            Kinds    : constant JSON_Array := Element.Get ("kinds");
            Kind     : constant String     := JSON_To_String (Get (Kinds, Length (Kinds)));
         begin
            Name := Name & ":" & To_Unbounded_String (Kind);
         end;
      end if;

      return To_String (Name);
   end Get_Node_Id;

   --------------------------------------------------------------------------------------------------------------------
   -- High Level : sub-program visible from outside
   --------------------------------------------------------------------------------------------------------------------

   --------------------------------
   -- Create_Nil_Parameters_Info --
   --------------------------------

   function Create_Nil_Parameters_Info return Parameters_Info is
   begin
      return Nil_Parameters_Info;
   end Create_Nil_Parameters_Info;

   ----------------------------
   -- Create_Parameters_Info --
   ----------------------------

   function Create_Parameters_Info (Call : Asis.Element; Normalized : Boolean := False) return Parameters_Info is
      use Asis.Elements, Asis.Expressions;

      Elements : Vector   := Empty_Vector;
      Index    : Positive := 1;
   begin
      for Parameter : Asis.Association of Thick_Queries.Actual_Parameters (Element    => Call,
                                                                           Normalized => False)
      loop
         Elements.Append ((Formal_Name                    => Formal_Parameter (Parameter),
                           Index                          => Index,
                           Is_Named_Parameter_Association => not Is_Nil (Formal_Parameter (Parameter))));
         Index := Index + 1;
      end loop;
      return (Nil_Parameter_Association, Elements => Elements, Current_Primitive_Index => 1, Normalized => Normalized);
   end Create_Parameters_Info;

   ----------------
   -- Enter_Unit --
   ----------------

   -- This procedure is called before any processing of a library unit.
   -- Plug calls here for rules that need initialization for every unit.

   procedure Enter_Unit (Unit : in Asis.Compilation_Unit) is
      use Ada.Characters.Handling;
      use Asis, Asis.Compilation_Units, Asis.Elements, Asis.Text;
      use GNATCOLL.JSON;
      Kind                     : constant String       := "Compilation_Unit";
      Json_Element             : constant JSON_Value   := Create_Object;

      Kinds                    : JSON_Array;
      Enclosing_Relation_Props : JSON_Array;
   begin --  Pre_Procedure
      Append (Kinds, Create (Kind));

      Json_Element.Set_Field ("kinds", Kinds);
      Json_Element.Set_Field ("location", Get_Location (Unit));
      Json_Element.Set_Field ("content", Content_Substr (To_String (Unit_Full_Name (Unit))));
      Json_Element.Set_Field ("is_predefined_unit", Unit_Origin (Unit) = A_Predefined_Unit);
      Json_Element.Set_Field ("node_id", Get_Node_Id (Json_Element));
      Export_Manager.Notify (Json_Element);
   end Enter_Unit;


   --------------------------
   -- Exit_Context_Clauses --
   --------------------------

   -- This procedure is called after all context clauses have been traversed,
   -- before traversing the unit itself

   procedure Exit_Context_Clauses (Unit : in Asis.Compilation_Unit) is
   begin
--       rules.dependencies.exit_context_clauses (unit);
--        Log.Info ("Exit_Context_Clauses");
      null;
   end Exit_Context_Clauses;

   ---------------
   -- Exit_Unit --
   ---------------

   -- This procedure is called after all processing of a library unit.
   -- Plug calls here for rules that need finalization for every unit.

   procedure Exit_Unit (Unit : in Asis.Compilation_Unit) is
   begin
--       rules.max_size.     process_unit      (unit);
--       rules.with_clauses. process_unit_exit (unit);
--        Log.Info ("Exit_Unit");
      null;
   end Exit_Unit;

   -----------------
   -- Enter_Scope --
   -----------------

   -- This procedure is called whenever we enter (Pre_Procedure) into a
   -- construct where elements can be declared. Since this happen in various
   -- places in the tree and is likely to be quite common use, it is easier to
   -- plug calls here rather than in every place that might require it.

   procedure Enter_Scope (Element : in Asis.Element) is
   begin
--       rules.max_nesting.process_scope_enter (element);
--       rules.statements. process_scope_enter (element);
--        Log.Info ("Enter_Scope");
      null;
   end Enter_Scope;

   -----------------------
   -- Enter_Public_Part --
   -----------------------

   procedure Enter_Public_Part   (Element : in Asis.Element) is
      use Ada.Characters.Handling;
      use Asis, Asis.Compilation_Units, Asis.Elements, Asis.Text;
      use GNATCOLL.JSON;
      Kind                     : constant String       := "Public_Part";
      Json_Element             : constant JSON_Value   := Create_Object;
      Json_Location            : constant JSON_Value   := Get_Fixed_Location (Element);

      Json_Parent              : constant JSON_Value   := Create_Object;

      Kinds                    : JSON_Array;
      Enclosing_Relation_Props : JSON_Array;

      Unit                     : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (Element => Element);
   begin --  Pre_Procedure
      Append (Kinds, Create (Kind));

      -- Set parent
      Json_Parent.Set_Field ("location", Json_Location);
      Json_Parent.Set_Field ("kind", Get_Kind_Image (Element));

      Add_Property (Arr   => Enclosing_Relation_Props,
                    Key   => "index",
                    Value => 1);

      Json_Parent.Set_Field ("properties", Enclosing_Relation_Props);
      Json_Parent.Set_Field ("node_id", Get_Node_Id (Json_Parent));
      Json_Element.Set_Field (Export_Utils.IS_ENCLOSED_IN'Image, Json_Parent);

      Json_Element.Set_Field ("kinds", Kinds);
      Json_Element.Set_Field ("location", Json_Location);
      Json_Element.Set_Field ("content", Content_Substr (Kind));
      Json_Element.Set_Field ("enclosing_unit", To_String (Unit_Full_Name (Unit)));
      Json_Element.Set_Field ("element_kind",  Element_Kinds'Image (Element_Kind (Element)));
      Json_Element.Set_Field ("is_part_of_implicit",  Is_Part_Of_Implicit (Element));
      Json_Element.Set_Field ("is_part_of_inherited", Is_Part_Of_Inherited (Element));
      Json_Element.Set_Field ("is_part_of_instance", Is_Part_Of_Instance (Element));
      Json_Element.Set_Field ("special_case", Special_Cases'Image (Asis.Set_Get.Special_Case (Element)));
      Json_Element.Set_Field ("node_id", Get_Node_Id (Json_Element));
      Export_Manager.Notify (Json_Element);
   end Enter_Public_Part;

   ------------------------
   -- Enter_Private_Part --
   ------------------------

   procedure Enter_Private_Part   (Element : in Asis.Element) is
      use Ada.Characters.Handling;
      use Asis, Asis.Compilation_Units, Asis.Elements, Asis.Text;
      use GNATCOLL.JSON;
      Kind                     : constant String       := "Private_Part";
      Json_Element             : constant JSON_Value   := Create_Object;
      Json_Location            : constant JSON_Value   := Get_Fixed_Location (Element);

      Json_Parent              : constant JSON_Value   := Create_Object;

      Kinds                    : JSON_Array;
      Enclosing_Relation_Props : JSON_Array;

      Unit                     : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (Element => Element);
   begin --  Pre_Procedure
      Append (Kinds, Create (Kind));

      -- Set parent
      Json_Parent.Set_Field ("location", Json_Location);
      Json_Parent.Set_Field ("kind", Get_Kind_Image (Element));

      Json_Element.Set_Field (Export_Utils.IS_ENCLOSED_IN'Image, Json_Parent);
      Json_Element.Set_Field ("kinds", Kinds);

      Add_Property (Arr   => Enclosing_Relation_Props,
                    Key   => "index",
                    Value => 2);

      Json_Parent.Set_Field ("properties", Enclosing_Relation_Props);
      Json_Parent.Set_Field ("node_id", Get_Node_Id (Json_Parent));
      Json_Element.Set_Field (Export_Utils.IS_ENCLOSED_IN'Image, Json_Parent);

      Json_Element.Set_Field ("location", Json_Location);
      Json_Element.Set_Field ("content", Content_Substr (Kind));
      Json_Element.Set_Field ("enclosing_unit", To_String (Unit_Full_Name (Unit)));
      Json_Element.Set_Field ("element_kind",  Element_Kinds'Image (Element_Kind (Element)));
      Json_Element.Set_Field ("is_part_of_implicit",  Is_Part_Of_Implicit (Element));
      Json_Element.Set_Field ("is_part_of_inherited", Is_Part_Of_Inherited (Element));
      Json_Element.Set_Field ("is_part_of_instance", Is_Part_Of_Instance (Element));
      Json_Element.Set_Field ("special_case", Special_Cases'Image (Asis.Set_Get.Special_Case (Element)));
      Json_Element.Set_Field ("node_id", Get_Node_Id (Json_Element));
      Export_Manager.Notify (Json_Element);
   end Enter_Private_Part;

   ----------------
   -- Exit_Scope --
   ----------------

   -- This procedure is called whenever we exit (Post_Procedure) from a
   -- construct where elements can be declared. Since this happen in various
   -- places in the tree and is likely to be quite common use, it is easier to
   -- plug calls here rather than in every place that might require it.

   procedure Exit_Scope (Element : in Asis.Element) is
   begin
--       rules.max_nesting.            process_scope_exit (element);
--       rules.no_operator_usage.      process_scope_exit (element);
--       rules.object_declarations.    process_scope_exit;
--       rules.reduceable_scope.       process_scope_exit (element);
--       rules.statements.             process_scope_exit (element);
--       rules.type_initial_values.    process_scope_exit (element);
--       rules.unnecessary_use_clause. process_scope_exit (element);
--       rules.max_primitives.         process_scope_exit (element);
--        Log.Info ("Exit_Scope");
      null;
   end Exit_Scope;

   --------------------------
   -- Enter_Statement_List --
   --------------------------

   -- Plug calls here to rules that need to process a Sequence_of_statements.
   -- Element is one of the elements to which Thick_Queries.Statements applies
   -- (i.e. a statement container)
   procedure Enter_Statement_List (Element : in Asis.Element) is
   begin
--       rules.assignments. process_statement_container (element);
--        Log.Info ("Enter_Statement_List");
      null;
   end Enter_Statement_List;

   ---------------------------
   -- Enter_Subprogram_Call --
   ---------------------------

   procedure Enter_Subprogram_Call (Element : in Asis.Element; Normalized : Boolean := False)
   is
      use Ada.Characters.Handling;
      use Asis, Asis.Compilation_Units, Asis.Elements, Asis.Text;
      use GNATCOLL.JSON;
      Kind                     : constant String       := (if Normalized then "Normalized_Parameters" else "User_Parameters");
      Json_Element             : constant JSON_Value   := Create_Object;
      Json_Location            : constant JSON_Value   := Get_Fixed_Location (Element);

      Json_Parent              : constant JSON_Value   := Create_Object;

      Kinds                    : JSON_Array;
      Enclosing_Relation_Props : JSON_Array;

      Unit                     : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (Element => Element);
   begin --  Pre_Procedure
      Append (Kinds, Create (Kind));

      -- Set parent
      Json_Parent.Set_Field ("location", Json_Location);
      Json_Parent.Set_Field ("kind", Get_Kind_Image (Element));

      Json_Element.Set_Field (Export_Utils.IS_ENCLOSED_IN'Image, Json_Parent);
      Json_Element.Set_Field ("kinds", Kinds);

      if not Normalized then
         Add_Property (Arr   => Enclosing_Relation_Props,
                       Key   => "index",
                       Value => 2);
      end if;

      Json_Parent.Set_Field ("properties", Enclosing_Relation_Props);
      Json_Parent.Set_Field ("node_id", Get_Node_Id (Json_Parent));
      Json_Element.Set_Field (Export_Utils.IS_ENCLOSED_IN'Image, Json_Parent);

      Json_Element.Set_Field ("location", Json_Location);
      Json_Element.Set_Field ("content", Content_Substr (Kind));
      Json_Element.Set_Field ("enclosing_unit", To_String (Unit_Full_Name (Unit)));
      Json_Element.Set_Field ("element_kind",  Element_Kinds'Image (Element_Kind (Element)));
      Json_Element.Set_Field ("is_part_of_implicit",  Is_Part_Of_Implicit (Element));
      Json_Element.Set_Field ("is_part_of_inherited", Is_Part_Of_Inherited (Element));
      Json_Element.Set_Field ("is_part_of_instance", Is_Part_Of_Instance (Element));
      Json_Element.Set_Field ("special_case", Special_Cases'Image (Asis.Set_Get.Special_Case (Element)));
      Json_Element.Set_Field ("node_id", Get_Node_Id (Json_Element));
      Export_Manager.Notify (Json_Element);
   end Enter_Subprogram_Call;

   ---------------------
   -- True_Identifier --
   ---------------------

   --  Plug calls here to rules that need to process all occurrences
   --  of "true" identifiers, including operator symbols and
   --  enumeration literals, but excluding identifiers that are pragma
   --  names or attributes selectors

   procedure True_Identifier (Element                : in Asis.Expression;
                              Corresponding_Instanciation : out Asis.Declaration)
   is
      use Asis.Elements, Asis.Expressions;

   begin
      begin
         declare
            Elt : constant Asis.Element := Corresponding_Name_Declaration (Element);
         begin
            if Is_Part_Of_Instance (Elt) then
               Corresponding_Instanciation := Thick_Queries.First_Enclosing_Instantiation (Elt);
            end if;
         end;
      exception
         when Asis.Exceptions.ASIS_Inappropriate_Element =>
            -- Some elements does not have a declaration (like elements in standard)
            null;

      end;


      -- Corresponding_Name_Declaration

--       rules.directly_accessed_globals. process_identifier (element);
--       rules.entities.                  process_identifier (element);
--       rules.not_selected_name.         process_identifier (element);
--       rules.object_declarations.       process_identifier (element);
--       rules.reduceable_scope.          process_identifier (element);
--       rules.unnecessary_use_clause.    process_identifier (element);
--       rules.style.                     process_identifier (element);
--       rules.usage.                     process_identifier (element);
--       rules.with_clauses.              process_identifier (element);
--        Log.Info ("True_Identifier");
      null;
   end True_Identifier;

   -------------------
   -- Pre_Procedure --
   -------------------

   procedure Pre_Procedure (Element                     : in Asis.Element;
                            State                       : in out Info;
                            Corresponding_Instantiation : in Asis.Element := Asis.Nil_Element)
   is
      use Ada.Characters.Conversions, Ada.Strings.Unbounded;
      use Asis, Asis.Compilation_Units, Asis.Elements, Asis.Text;
      use GNATCOLL.JSON;

      Json_Element : constant JSON_Value := Create_Object;
      Kinds        : JSON_Array;

      procedure Set_Corresponding_Declaration_Type_Definition is
         use Thick_Queries;

         function Corresponding_Declaration_Type_Declaration (The_Declaration : Asis.Declaration)
                                                              return Asis.Declaration
         is
            use Asis.Declarations, Asis.Definitions, Asis.Expressions;

            Def : Asis.Definition;
         begin
            case Declaration_Kind (The_Declaration) is
            when A_Variable_Declaration
               | A_Constant_Declaration
               | A_Deferred_Constant_Declaration
               | A_Return_Variable_Specification
               | A_Return_Constant_Specification
               =>
               Def := Object_Declaration_View (The_Declaration);
               case Definition_Kind (Def) is
               when A_Type_Definition =>
                  -- This can only be an anonymous array => we have the definition
                  return Def; -- TODO
               when An_Access_Definition =>   -- ASIS 2005
                  -- Anonymous access type
                  return Def; -- TODO
               when others =>
                  return Corresponding_Name_Declaration
                    (Simple_Name
                       (Strip_Attributes   -- Ignore 'Base and 'Class if any
                            (Subtype_Simple_Name (Def))));
               end case;
            when A_Discriminant_Specification
               | A_Parameter_Specification
               =>
               Def := Object_Declaration_View (The_Declaration);
               if Definition_Kind (Def) = An_Access_Definition then
                  return Def; -- TODO
               end if;
               return Corresponding_Name_Declaration (Simple_Name (Strip_Attributes (Def)));
            when A_Loop_Parameter_Specification =>
               return Specification_Subtype_Definition (The_Declaration); -- TODO
            when A_Formal_Object_Declaration =>
               if Definition_Kind (Object_Declaration_View (The_Declaration)) = An_Access_Definition then
                  -- Must be an anonymous formal type
                  return Object_Declaration_View (The_Declaration); -- TODO
               else
                  declare
                     Subtype_Mark : Asis.Expression := Declaration_Subtype_Mark (The_Declaration);
                  begin
                     if Expression_Kind (Subtype_Mark) = A_Selected_Component then
                        Subtype_Mark := Selector (Subtype_Mark);
                     end if;
                     if Expression_Kind (Subtype_Mark) = An_Attribute_Reference then
                        Subtype_Mark := Prefix (Subtype_Mark);
                     end if;
                     return  Corresponding_Name_Declaration (Subtype_Mark);
                  end;
               end if;
            when A_Component_Declaration =>
               Def := Component_Definition_View (Object_Declaration_View (The_Declaration));   -- ASIS 2005
               if Definition_Kind (Def) = An_Access_Definition then -- ASIS 2005
                  -- A component whose type is an anonymous access type
                  return Def; -- TODO
               else
                  declare
                     Expr : constant Asis.Expression :=
                       Asis.Definitions.Subtype_Mark (Simple_Name (Strip_Attributes (Def)));
                  begin
                     case Expression_Kind (Expr) is
                     when A_Selected_Component =>
                        return Corresponding_Name_Declaration (Selector (Expr));
                     when An_Attribute_Reference =>
                        return Nil_Element;
                     when others =>
                        return Corresponding_Name_Declaration (Expr);
                     end case;
                  end;
               end if;
            when A_Single_Protected_Declaration
               | A_Single_Task_Declaration
               | An_Object_Renaming_Declaration
               =>
               return Nil_Element;
            when others =>
               return Nil_Element;
            end case;
         end Corresponding_Declaration_Type_Declaration;

         Def : constant Asis.Definition := Corresponding_Declaration_Type_Declaration (Element);
      begin
         if Element_Kind (Def) = Not_An_Element then
            return;
         end if;

         declare
            Json_Def          : constant JSON_Value   := Create_Object;
         begin
            Json_Def.Set_Field ("location", Get_Fixed_Location (Def));
            Json_Def.Set_Field ("kind", Get_Kind_Image (Def));
            Json_Def.Set_Field ("node_id", Get_Node_Id (Json_Def));
            Json_Element.Set_Field (Export_Utils.IS_OF_TYPE'Image, Json_Def);
         end;
      end Set_Corresponding_Declaration_Type_Definition;

      procedure Set_Corresponding_Type_Declaration_View is
         use Thick_Queries;
         use Asis.Declarations;

         Def : constant Asis.Definition := Type_Declaration_View (Element);
      begin
         if Element_Kind (Def) = Not_An_Element then
            return;
         end if;

         declare
            Json_Def : constant JSON_Value := Create_Object;
         begin
            Json_Def.Set_Field ("location", Get_Fixed_Location (Def));
            Json_Def.Set_Field ("kind", Get_Kind_Image (Def));
            Json_Def.Set_Field ("node_id", Get_Node_Id (Json_Def));
            Json_Element.Set_Field (Export_Utils.CORRESPONDING_TYPE_DECLARATION_VIEW'Image, Json_Def);
         end;
      end Set_Corresponding_Type_Declaration_View;

      procedure Set_Corresponding_Root_Type is
         use Thick_Queries;
         use Asis.Definitions;

         Def : constant Asis.Definition := Corresponding_Root_Type (Element);
      begin
         if Element_Kind (Def) = Not_An_Element then
            return;
         end if;

         declare
            Json_Def : constant JSON_Value := Create_Object;
         begin
            Json_Def.Set_Field ("location", Get_Fixed_Location (Def));
            Json_Def.Set_Field ("kind", Get_Kind_Image (Def));
            Json_Def.Set_Field ("node_id", Get_Node_Id (Json_Def));
            Json_Element.Set_Field (Export_Utils.CORRESPONDING_ROOT_TYPE'Image, Json_Def);
         end;
      end Set_Corresponding_Root_Type;

      procedure Set_Corresponding_Name_Definition is
         CND    : Asis.Element;
         Parent : constant Asis.Element := Enclosing_Element (Element);
      begin
         if Expression_Kind (Parent) = An_Attribute_Reference then
            return;
         end if;
         CND := Asis.Expressions.Corresponding_Name_Definition (Element);
         if Element_Kind (CND) = Not_An_Element then
            -- In some cases there is no definition
            return;
         end if;

         declare
            Json_CND : constant JSON_Value := Create_Object;
         begin
            Json_CND.Set_Field ("location", Get_Fixed_Location (CND));
            Json_CND.Set_Field ("kind", Get_Kind_Image (CND));
            Json_CND.Set_Field ("node_id", Get_Node_Id (Json_CND));
            Json_Element.Set_Field (Export_Utils.CORRESPONDING_NAME_DEFINITION'Image, Json_CND);
         end;
      exception
         when E : Asis.Exceptions.Asis_Inappropriate_Element =>
            --  In some cases, there is no declaration
            --  Take a look at the end of the Asis.Expressions.Corresponding_Name_Definition
            return;
      end Set_Corresponding_Name_Definition;

      procedure Set_Trait_Kind is
      begin
         case Trait_Kind (Element) is
         when A_Limited_Private_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (A_Limited_Private_Trait)));
         when A_Limited_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (A_Limited_Trait)));
         when A_Null_Exclusion_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (A_Null_Exclusion_Trait)));
         when A_Private_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (A_Private_Trait)));
         when A_Reverse_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (A_Reverse_Trait)));
         when An_Abstract_Limited_Private_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (An_Abstract_Limited_Private_Trait)));
         when An_Abstract_Limited_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (An_Abstract_Limited_Trait)));
         when An_Abstract_Private_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (An_Abstract_Private_Trait)));
         when An_Abstract_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (An_Abstract_Trait)));
         when An_Access_Definition_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (An_Access_Definition_Trait)));
         when An_Aliased_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (An_Aliased_Trait)));
         when An_Ordinary_Trait =>
            Append (Kinds, Create (Trait_Kinds'Image (An_Ordinary_Trait)));
         when Not_A_Trait =>
            --  Nothing to do here
            null;
         end case;
      end Set_Trait_Kind;

      --  Create a relation between the current Element and the corresponding specification of a subprogram, package,
      --  or task body declaration or an expression function declaration.
      procedure Set_Corresponding_Specification is
      begin
         if Declaration_Kind (Element) not in A_Formal_Package_Declaration | A_Formal_Package_Declaration_With_Box |
               A_Function_Body_Declaration | A_Function_Body_Stub | A_Function_Declaration | A_Function_Instantiation |
               A_Function_Renaming_Declaration | A_Generic_Function_Declaration |
               A_Generic_Function_Renaming_Declaration | A_Generic_Package_Declaration |
               A_Generic_Package_Renaming_Declaration | A_Generic_Procedure_Declaration |
               A_Generic_Procedure_Renaming_Declaration | A_Null_Procedure_Declaration | A_Package_Body_Declaration |
               A_Package_Body_Stub | A_Package_Declaration | A_Package_Instantiation | A_Package_Renaming_Declaration |
               A_Procedure_Body_Declaration | A_Procedure_Body_Stub | A_Procedure_Declaration |
               A_Procedure_Instantiation | A_Procedure_Renaming_Declaration | A_Protected_Body_Declaration |
               A_Protected_Body_Stub | A_Protected_Type_Declaration | A_Single_Protected_Declaration |
               A_Single_Task_Declaration | A_Task_Body_Declaration | A_Task_Body_Stub | A_Task_Type_Declaration |
               An_Entry_Body_Declaration | An_Entry_Declaration | An_Expression_Function_Declaration
         then
            -- See {@link Asis.Declarations.Corresponding_Declaration} for more details
            return;
         end if;

         declare
            Json_Def : constant JSON_Value := Create_Object;
            Decl     : Asis.Element := Asis.Declarations.Corresponding_Declaration (Element);
         begin
            if Is_Nil (Decl) then
               Decl := Element;
            end if;
            Json_Def.Set_Field ("location", Get_Fixed_Location (Decl));
            Json_Def.Set_Field ("kind", Get_Kind_Image (Decl));
            Json_Def.Set_Field ("node_id", Get_Node_Id (Json_Def));
            Json_Element.Set_Field (Export_Utils.CORRESPONDING_SPECIFICATION'Image, Json_Def);
         end;
      end Set_Corresponding_Specification;

      procedure Set_Corresponding_First_Subtype is
         use Thick_Queries;
         use Asis.Declarations;

         Def : constant Asis.Definition := Corresponding_First_Subtype (Element);
      begin
         declare
            Json_Def : constant JSON_Value := Create_Object;
         begin
            Json_Def.Set_Field ("location", Get_Fixed_Location (Def));
            Json_Def.Set_Field ("kind", Get_Kind_Image (Def));
            Json_Def.Set_Field ("node_id", Get_Node_Id (Json_Def));
            Json_Element.Set_Field (Export_Utils.CORRESPONDING_FIRST_SUBTYPE'Image, Json_Def);
         end;
      end Set_Corresponding_First_Subtype;

      Json_Location        : constant JSON_Value   := Create_Object;
      Loc                  : constant Location     := Get_Location (Element);

      Parent               : constant Asis.Element := Enclosing_Element (Element);
      Parent_Loc           : constant Location     := Get_Fixed_Location (Parent);
      Json_Parent_Location : constant JSON_Value   := Create_Object;
      Json_Parent          : constant JSON_Value   := Create_Object;

      Unit                 : constant Asis.Compilation_Unit := Enclosing_Compilation_Unit (Element);

      Parent_UUID          : Unbounded_String := Null_Unbounded_String;
   begin --  Pre_Procedure
      Append (Kinds, Create ("Element"));

      Json_Location.Set_Field ("filename", To_String (Get_File_Name (loc)));
      Json_Location.Set_Field ("line", Get_First_Line (loc));
      Json_Location.Set_Field ("column", Get_First_Column (loc));

      if Is_Nil (Parent) then
         Json_Parent.Set_Field ("location", Get_Location (Unit));

         if Asis.Set_Get.Special_Case (Element) = CONFIGURATION_FILE_PRAGMA then
            Json_Parent.Set_Field ("kind", "Configuration_File_Pragma");
         else
            Json_Parent.Set_Field ("kind", "Compilation_Unit");
         end if;
      else
         Json_Parent_Location.Set_Field ("filename", To_String (Get_File_Name (Parent_Loc)));
         Json_Parent_Location.Set_Field ("line", Get_First_Line (Parent_Loc));
         Json_Parent_Location.Set_Field ("column", Get_First_Column (Parent_Loc));
         Json_Parent.Set_Field ("location", Json_Parent_Location);

         if Association_Kind (Element) = A_Parameter_Association
           and then (Statement_Kind (Parent) in A_Procedure_Call_Statement | An_Entry_Call_Statement
                     or Expression_Kind (Parent) in  A_Function_Call)
         then
            if State.Parameters_Information.Normalized then
               Json_Parent.Set_Field ("kind", "Normalized_Parameters");

               -- Set the location of the current node to the location of the parent (filename, line) because
               -- Normalized node do not have a location.
               Json_Location.Set_Field ("filename", To_String (Get_File_Name (Parent_Loc)));
               Json_Location.Set_Field ("line", Get_First_Line (Parent_Loc));
            else
               Json_Parent.Set_Field ("kind", "User_Parameters");
            end if;
         elsif Declaration_Kind (Parent) in A_Package_Declaration | A_Generic_Package_Declaration then
            if State.Visibility = V_Public then
               Json_Parent.Set_Field ("kind", "Public_Part");
            else
               Json_Parent.Set_Field ("kind", "Private_Part");
            end if;
         else
            Json_Parent.Set_Field ("kind", Get_Kind_Image (Parent));
         end if;
      end if;

      Parent_UUID := To_Unbounded_String (Get_Node_Id (Json_Parent));
      Json_Parent.Set_Field ("node_id", To_String (Parent_UUID));

      if not Is_Nil (Corresponding_Instantiation) then
         declare
            Json_Elt : constant JSON_Value := Create_Object;
         begin
            Json_Elt.Set_Field ("location", Get_Fixed_Location (Corresponding_Instantiation));
            Json_Elt.Set_Field ("kind", Get_Kind_Image (Corresponding_Instantiation));
            Json_Elt.Set_Field ("node_id", Get_Node_Id (Json_Elt));
            Json_Element.Set_Field (Export_Utils.CORRESPONDING_INSTANTIATION'Image, Json_Elt);
         end;
      end if;

      if State.Parameters_Information /= Nil_Parameters_Info
        and then not State.Parameters_Information.Normalized
        and then State.Parameters_Information.Association /= Nil_Parameter_Association
        and then State.Parameters_Information.Association.Corresponding_Actual_Expression.Get ("location") = Json_Location
        and then Standard."=" (State.Parameters_Information.Association.Corresponding_Actual_Expression.Get ("kind"), Get_Kind_Image (Element))
      then
         declare
            JSON_Elt : constant JSON_Value := Clone (State.Parameters_Information.Association.Association);
         begin
            JSON_Elt.Set_Field ("node_id", Get_Node_Id (JSON_Elt));
            Json_Element.Set_Field (Export_Utils.CORRESPONDING_ACTUAL_PARAMETER'Image, JSON_Elt);
            State.Parameters_Information.Association := Nil_Parameter_Association;
         end;
      end if;

      Json_Element.Set_Field ("location", Json_Location);
      Json_Element.Set_Field
        ("content", Content_Substr (Ada.Strings.Fixed.Trim (To_String (Element_Image (Element)), Ada.Strings.Both)));
      Json_Element.Set_Field ("enclosing_unit", To_String (Unit_Full_Name (Unit)));
      Json_Element.Set_Field ("element_kind",  Element_Kinds'Image (Element_Kind (Element)));
      Json_Element.Set_Field ("is_part_of_implicit",  Is_Part_Of_Implicit (Element));
      Json_Element.Set_Field ("is_part_of_inherited", Is_Part_Of_Inherited (Element));
      Json_Element.Set_Field ("is_part_of_instance", Is_Part_Of_Instance (Element));
      Json_Element.Set_Field ("special_case", Special_Cases'Image (Asis.Set_Get.Special_Case (Element)));

      if Asis.Set_Get.Special_Case (Element) = CONFIGURATION_FILE_PRAGMA and not Config_File_Map.Contains (Parent_UUID)
      then
         -- In the case of config file (e.g. gnat.adc), we do not enter into the unit,
         -- because it is not a compilation unit as such, so we create the node into the DB,
         -- before adding current node.
         Config_File_Map.Insert (Parent_UUID, True);
         Export_Manager.Notify (Json_Parent);
      end if;

      case Element_Kind (Element) is
      when A_Clause =>
         Append (Kinds, Create (Element_Kinds'Image (A_Clause)));
         case Clause_Kind (Element) is
         when A_Component_Clause =>
            Log.Info ("[Pre_Procedure] A_Clause > A_Component_Clause");
            Append (Kinds, Create (Clause_Kinds'Image (A_Component_Clause)));

         when A_Representation_Clause =>
            Append (Kinds, Create (Clause_Kinds'Image (A_Representation_Clause)));
            case Representation_Clause_Kind (Element) is
               when A_Record_Representation_Clause =>
                  Log.Info ("[Pre_Procedure] A_Clause > A_Representation_Clause > A_Record_Representation_Clause");
                  Append (Kinds, Create (Representation_Clause_Kinds'Image (A_Record_Representation_Clause)));

               when An_At_Clause =>
                  Log.Info ("[Pre_Procedure] A_Clause > A_Representation_Clause > An_At_Clause");
                  Append (Kinds, Create (Representation_Clause_Kinds'Image (An_At_Clause)));

               when An_Attribute_Definition_Clause =>
                  Log.Info ("[Pre_Procedure] A_Clause > A_Representation_Clause > An_Attribute_Definition_Clause");
                  Append (Kinds, Create (Representation_Clause_Kinds'Image (An_Attribute_Definition_Clause)));

               when An_Enumeration_Representation_Clause =>
                  Log.Info ("[Pre_Procedure] A_Clause > A_Representation_Clause > An_Enumeration_Representation_Clause");
                  Append (Kinds, Create (Representation_Clause_Kinds'Image (An_Enumeration_Representation_Clause)));

               when Not_A_Representation_Clause =>
                  --  Nothing to do here
                  null;
            end case;

         when A_Use_All_Type_Clause =>
            Log.Info ("[Pre_Procedure] A_Clause > A_Use_All_Type_Clause");
            Append (Kinds, Create (Clause_Kinds'Image (A_Use_All_Type_Clause)));

         when A_Use_Package_Clause =>
            Log.Info ("[Pre_Procedure] A_Clause > A_Use_Package_Clause");
            Append (Kinds, Create (Clause_Kinds'Image (A_Use_Package_Clause)));

         when A_Use_Type_Clause =>
            Log.Info ("[Pre_Procedure] A_Clause > A_Use_Type_Clause");
            Append (Kinds, Create (Clause_Kinds'Image (A_Use_Type_Clause)));

         when A_With_Clause =>
            Log.Info ("[Pre_Procedure] A_Clause > A_With_Clause");
            Append (Kinds, Create (Clause_Kinds'Image (A_With_Clause)));
            Set_Trait_Kind;

         when Not_A_Clause =>
            --  Nothing to do here
            null;
         end case;

      when A_Declaration =>
         Append (Kinds, Create (Element_Kinds'Image (A_Declaration)));
         Set_Corresponding_Specification;
         case Declaration_Kind (Element) is
         when A_Choice_Parameter_Specification =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Choice_Parameter_Specification");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Choice_Parameter_Specification)));

         when A_Component_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Component_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Component_Declaration)));
            Set_Corresponding_Declaration_Type_Definition;

         when A_Constant_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Constant_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Constant_Declaration)));
            Set_Corresponding_Declaration_Type_Definition;
            Set_Trait_Kind;

         when A_Deferred_Constant_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Deferred_Constant_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Deferred_Constant_Declaration)));
            Set_Corresponding_Declaration_Type_Definition;
            Set_Trait_Kind;

         when A_Discriminant_Specification =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Discriminant_Specification");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Discriminant_Specification)));
            Set_Corresponding_Declaration_Type_Definition;
            Set_Trait_Kind;

         when A_Formal_Function_Declaration =>
            Append (Kinds, Create (Declaration_Kinds'Image (A_Formal_Function_Declaration)));
            case Default_Kind (Element) is
               when A_Box_Default =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Function_Declaration > A_Box_Default");
                  Append (Kinds, Create (Subprogram_Default_Kinds'Image (A_Box_Default)));

               when A_Name_Default =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Function_Declaration > A_Name_Default");
                  Append (Kinds, Create (Subprogram_Default_Kinds'Image (A_Name_Default)));

               when A_Nil_Default =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Function_Declaration > A_Nil_Default");
                  Append (Kinds, Create (Subprogram_Default_Kinds'Image (A_Nil_Default)));

               when A_Null_Default =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Function_Declaration > A_Null_Default");
                  Append (Kinds, Create (Subprogram_Default_Kinds'Image (A_Null_Default)));

               when Not_A_Default =>
                  --  Nothing to do here
                  null;
            end case;
            Set_Trait_Kind;

         when A_Formal_Incomplete_Type_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Incomplete_Type_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Formal_Incomplete_Type_Declaration)));
            Set_Corresponding_First_Subtype;

         when A_Formal_Object_Declaration =>
            Append (Kinds, Create (Declaration_Kinds'Image (A_Formal_Object_Declaration)));
            case Mode_Kind (Element) is
               when A_Default_In_Mode =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Object_Declaration > A_Default_In_Mode");
                  Append (Kinds, Create (Mode_Kinds'Image (A_Default_In_Mode)));

               when An_In_Mode =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Object_Declaration > An_In_Mode");
                  Append (Kinds, Create (Mode_Kinds'Image (An_In_Mode)));

               when An_In_Out_Mode =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Object_Declaration > An_In_Out_Mode");
                  Append (Kinds, Create (Mode_Kinds'Image (An_In_Out_Mode)));

               when An_Out_Mode =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Object_Declaration > An_Out_Mode");
                  Append (Kinds, Create (Mode_Kinds'Image (An_Out_Mode)));

               when Not_A_Mode =>
                  --  Nothing to do here
                  null;
            end case;
            Set_Corresponding_Declaration_Type_Definition;
            Set_Trait_Kind;

         when A_Formal_Package_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Package_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Formal_Package_Declaration)));

         when A_Formal_Package_Declaration_With_Box =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Package_Declaration_With_Box");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Formal_Package_Declaration_With_Box)));

         when A_Formal_Procedure_Declaration =>
            Append (Kinds, Create (Declaration_Kinds'Image (A_Formal_Procedure_Declaration)));
            case Default_Kind (Element) is
               when A_Box_Default =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Procedure_Declaration > A_Box_Default");
                  Append (Kinds, Create (Subprogram_Default_Kinds'Image (A_Box_Default)));

               when A_Name_Default =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Procedure_Declaration > A_Name_Default");
                  Append (Kinds, Create (Subprogram_Default_Kinds'Image (A_Name_Default)));

               when A_Nil_Default =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Procedure_Declaration > A_Nil_Default");
                  Append (Kinds, Create (Subprogram_Default_Kinds'Image (A_Nil_Default)));

               when A_Null_Default =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Procedure_Declaration > A_Null_Default");
                  Append (Kinds, Create (Subprogram_Default_Kinds'Image (A_Null_Default)));

               when Not_A_Default =>
                  --  Nothing to do here
                  null;
            end case;
            Set_Trait_Kind;

         when A_Formal_Type_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Formal_Type_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Formal_Type_Declaration)));
            Set_Corresponding_Type_Declaration_View;
            Set_Corresponding_First_Subtype;

         when A_Function_Body_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Function_Body_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Function_Body_Declaration)));

         when A_Function_Body_Stub =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Function_Body_Stub");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Function_Body_Stub)));

         when A_Function_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Function_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Function_Declaration)));
            Set_Trait_Kind;

         when A_Function_Instantiation =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Function_Instantiation");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Function_Instantiation)));

         when A_Function_Renaming_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Function_Renaming_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Function_Renaming_Declaration)));

         when A_Generalized_Iterator_Specification =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Generalized_Iterator_Specification");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Generalized_Iterator_Specification)));
            Set_Trait_Kind;

         when A_Generic_Function_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Generic_Function_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Generic_Function_Declaration)));

         when A_Generic_Function_Renaming_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Generic_Function_Renaming_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Generic_Function_Renaming_Declaration)));

         when A_Generic_Package_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Generic_Package_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Generic_Package_Declaration)));

         when A_Generic_Package_Renaming_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Generic_Package_Renaming_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Generic_Package_Renaming_Declaration)));

         when A_Generic_Procedure_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Generic_Procedure_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Generic_Procedure_Declaration)));

         when A_Generic_Procedure_Renaming_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Generic_Procedure_Renaming_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Generic_Procedure_Renaming_Declaration)));

         when A_Loop_Parameter_Specification =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Loop_Parameter_Specification");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Loop_Parameter_Specification)));
            Set_Corresponding_Declaration_Type_Definition;
            Set_Trait_Kind;

         when A_Null_Procedure_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Null_Procedure_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Null_Procedure_Declaration)));

         when A_Package_Body_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Package_Body_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Package_Body_Declaration)));

         when A_Package_Body_Stub =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Package_Body_Stub");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Package_Body_Stub)));

         when A_Package_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Package_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Package_Declaration)));

         when A_Package_Instantiation =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Package_Instantiation");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Package_Instantiation)));

         when A_Package_Renaming_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Package_Renaming_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Package_Renaming_Declaration)));

         when A_Parameter_Specification =>
            Append (Kinds, Create (Declaration_Kinds'Image (A_Parameter_Specification)));
            case Mode_Kind (Element) is
               when A_Default_In_Mode =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Parameter_Specification > A_Default_In_Mode");
                  Append (Kinds, Create (Mode_Kinds'Image (A_Default_In_Mode)));

               when An_In_Mode =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Parameter_Specification > An_In_Mode");
                  Append (Kinds, Create (Mode_Kinds'Image (An_In_Mode)));

               when An_In_Out_Mode =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Parameter_Specification > An_In_Out_Mode");
                  Append (Kinds, Create (Mode_Kinds'Image (An_In_Out_Mode)));

               when An_Out_Mode =>
                  Log.Info ("[Pre_Procedure] A_Declaration > A_Parameter_Specification > An_Out_Mode");
                  Append (Kinds, Create (Mode_Kinds'Image (An_Out_Mode)));

               when Not_A_Mode =>
                  --  Nothing to do here
                  null;
            end case;
            Set_Corresponding_Declaration_Type_Definition;
            Set_Trait_Kind;

         when A_Private_Extension_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Private_Extension_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Private_Extension_Declaration)));
            Set_Trait_Kind;
            Set_Corresponding_Type_Declaration_View;
            Set_Corresponding_First_Subtype;

         when A_Private_Type_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Private_Type_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Private_Type_Declaration)));
            Set_Trait_Kind;
            Set_Corresponding_Type_Declaration_View;
            Set_Corresponding_First_Subtype;

         when A_Procedure_Body_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Procedure_Body_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Procedure_Body_Declaration)));

         when A_Procedure_Body_Stub =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Procedure_Body_Stub");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Procedure_Body_Stub)));

         when A_Procedure_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Procedure_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Procedure_Declaration)));
            Set_Trait_Kind;

         when A_Procedure_Instantiation =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Procedure_Instantiation");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Procedure_Instantiation)));

         when A_Procedure_Renaming_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Procedure_Renaming_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Procedure_Renaming_Declaration)));

         when A_Protected_Body_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Protected_Body_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Protected_Body_Declaration)));

         when A_Protected_Body_Stub =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Protected_Body_Stub");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Protected_Body_Stub)));

         when A_Protected_Type_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Protected_Type_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Protected_Type_Declaration)));
            Set_Corresponding_Type_Declaration_View;
            Set_Corresponding_First_Subtype;

         when A_Real_Number_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Real_Number_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Real_Number_Declaration)));

         when A_Return_Constant_Specification =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Return_Constant_Specification");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Return_Constant_Specification)));
            Set_Corresponding_Declaration_Type_Definition;

         when A_Return_Variable_Specification =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Return_Variable_Specification");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Return_Variable_Specification)));
            Set_Corresponding_Declaration_Type_Definition;

         when A_Single_Protected_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Single_Protected_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Single_Protected_Declaration)));
            Set_Corresponding_Declaration_Type_Definition;

         when A_Single_Task_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Single_Task_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Single_Task_Declaration)));
            Set_Corresponding_Declaration_Type_Definition;

         when A_Subtype_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Subtype_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Subtype_Declaration)));
            Set_Corresponding_Type_Declaration_View;
            Set_Corresponding_First_Subtype;

         when A_Tagged_Incomplete_Type_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Tagged_Incomplete_Type_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Tagged_Incomplete_Type_Declaration)));
            Set_Corresponding_First_Subtype;

         when A_Task_Body_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Task_Body_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Task_Body_Declaration)));

         when A_Task_Body_Stub =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Task_Body_Stub");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Task_Body_Stub)));

         when A_Task_Type_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Task_Type_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Task_Type_Declaration)));
            Set_Corresponding_Type_Declaration_View;
            Set_Corresponding_First_Subtype;

         when A_Variable_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > A_Variable_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (A_Variable_Declaration)));
            Set_Corresponding_Declaration_Type_Definition;
            Set_Trait_Kind;

         when An_Element_Iterator_Specification =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Element_Iterator_Specification");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Element_Iterator_Specification)));
            Set_Corresponding_Declaration_Type_Definition;
            Set_Trait_Kind;

         when An_Entry_Body_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Entry_Body_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Entry_Body_Declaration)));

         when An_Entry_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Entry_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Entry_Declaration)));

         when An_Entry_Index_Specification =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Entry_Index_Specification");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Entry_Index_Specification)));

         when An_Enumeration_Literal_Specification =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Enumeration_Literal_Specification");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Enumeration_Literal_Specification)));

         when An_Exception_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Exception_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Exception_Declaration)));

         when An_Exception_Renaming_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Exception_Renaming_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Exception_Renaming_Declaration)));

         when An_Expression_Function_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Expression_Function_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Expression_Function_Declaration)));

         when An_Incomplete_Type_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Incomplete_Type_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Incomplete_Type_Declaration)));
            Set_Corresponding_First_Subtype;

         when An_Integer_Number_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Integer_Number_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Integer_Number_Declaration)));

         when An_Object_Renaming_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Object_Renaming_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Object_Renaming_Declaration)));
            Set_Corresponding_Declaration_Type_Definition;
            Set_Trait_Kind;

         when An_Ordinary_Type_Declaration =>
            Log.Info ("[Pre_Procedure] A_Declaration > An_Ordinary_Type_Declaration");
            Append (Kinds, Create (Declaration_Kinds'Image (An_Ordinary_Type_Declaration)));
            Set_Corresponding_Type_Declaration_View;
            Set_Corresponding_First_Subtype;

         when Not_A_Declaration =>
            --  Nothing to do here
            null;
         end case;

      when A_Defining_Name =>
         Append (Kinds, Create (Element_Kinds'Image (A_Defining_Name)));
         case Defining_Name_Kind (Element) is
         when A_Defining_Character_Literal =>
            Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Character_Literal");
            Append (Kinds, Create (Defining_Name_Kinds'Image (A_Defining_Character_Literal)));

         when A_Defining_Enumeration_Literal =>
            Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Enumeration_Literal");
            Append (Kinds, Create (Defining_Name_Kinds'Image (A_Defining_Enumeration_Literal)));

         when A_Defining_Expanded_Name =>
            Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Expanded_Name");
            Append (Kinds, Create (Defining_Name_Kinds'Image (A_Defining_Expanded_Name)));

         when A_Defining_Identifier =>
            Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Identifier");
            Append (Kinds, Create (Defining_Name_Kinds'Image (A_Defining_Identifier)));

         when A_Defining_Operator_Symbol =>
            Append (Kinds, Create (Defining_Name_Kinds'Image (A_Defining_Operator_Symbol)));
            case Operator_Kind (Element) is
               when A_Concatenate_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Concatenate_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Concatenate_Operator)));

               when A_Divide_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Divide_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Divide_Operator)));

               when A_Greater_Than_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Greater_Than_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Greater_Than_Operator)));

               when A_Greater_Than_Or_Equal_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Greater_Than_Or_Equal_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Greater_Than_Or_Equal_Operator)));

               when A_Less_Than_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Less_Than_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Less_Than_Operator)));

               when A_Less_Than_Or_Equal_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Less_Than_Or_Equal_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Less_Than_Or_Equal_Operator)));

               when A_Minus_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Minus_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Minus_Operator)));

               when A_Mod_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Mod_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Mod_Operator)));

               when A_Multiply_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Multiply_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Multiply_Operator)));

               when A_Not_Equal_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Not_Equal_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Not_Equal_Operator)));

               when A_Not_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Not_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Not_Operator)));

               when A_Plus_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Plus_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Plus_Operator)));

               when A_Rem_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Rem_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Rem_Operator)));

               when A_Unary_Minus_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Unary_Minus_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Unary_Minus_Operator)));

               when A_Unary_Plus_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > A_Unary_Plus_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Unary_Plus_Operator)));

               when An_Abs_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > An_Abs_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_Abs_Operator)));

               when An_And_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > An_And_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_And_Operator)));

               when An_Equal_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > An_Equal_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_Equal_Operator)));

               when An_Exponentiate_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > An_Exponentiate_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_Exponentiate_Operator)));

               when An_Or_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > An_Or_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_Or_Operator)));

               when An_Xor_Operator =>
                  Log.Info ("[Pre_Procedure] A_Defining_Name > A_Defining_Operator_Symbol > An_Xor_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_Xor_Operator)));

               when Not_An_Operator =>
                  --  Nothing to do here
                  null;
            end case;

         when Not_A_Defining_Name =>
            --  Nothing to do here
            null;
         end case;

      when A_Definition =>
         Append (Kinds, Create (Element_Kinds'Image (A_Definition)));
         case Definition_Kind (Element) is
         when A_Component_Definition =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Component_Definition");
            Append (Kinds, Create (Definition_Kinds'Image (A_Component_Definition)));
            Set_Trait_Kind;

         when A_Constraint =>
            Append (Kinds, Create (Definition_Kinds'Image (A_Constraint)));
            case Constraint_Kind (Element) is
               when A_Delta_Constraint =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Constraint > A_Delta_Constraint");
                  Append (Kinds, Create (Constraint_Kinds'Image (A_Delta_Constraint)));

               when A_Digits_Constraint =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Constraint > A_Digits_Constraint");
                  Append (Kinds, Create (Constraint_Kinds'Image (A_Digits_Constraint)));

               when A_Discriminant_Constraint =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Constraint > A_Discriminant_Constraint");
                  Append (Kinds, Create (Constraint_Kinds'Image (A_Discriminant_Constraint)));

               when A_Range_Attribute_Reference =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Constraint > A_Range_Attribute_Reference");
                  Append (Kinds, Create (Constraint_Kinds'Image (A_Range_Attribute_Reference)));

               when A_Simple_Expression_Range =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Constraint > A_Simple_Expression_Range");
                  Append (Kinds, Create (Constraint_Kinds'Image (A_Simple_Expression_Range)));

               when An_Index_Constraint =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Constraint > An_Index_Constraint");
                  Append (Kinds, Create (Constraint_Kinds'Image (An_Index_Constraint)));

               when Not_A_Constraint =>
                  --  Nothing to do here
                  null;
            end case;

         when A_Discrete_Range =>
            Append (Kinds, Create (Definition_Kinds'Image (A_Discrete_Range)));
            case Discrete_Range_Kind (Element) is
               when A_Discrete_Range_Attribute_Reference =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Discrete_Range > A_Discrete_Range_Attribute_Reference");
                  Append (Kinds, Create (Discrete_Range_Kinds'Image (A_Discrete_Range_Attribute_Reference)));

               when A_Discrete_Simple_Expression_Range =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Discrete_Range > A_Discrete_Simple_Expression_Range");
                  Append (Kinds, Create (Discrete_Range_Kinds'Image (A_Discrete_Simple_Expression_Range)));

               when A_Discrete_Subtype_Indication =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Discrete_Range > A_Discrete_Subtype_Indication");
                  Append (Kinds, Create (Discrete_Range_Kinds'Image (A_Discrete_Subtype_Indication)));

               when Not_A_Discrete_Range =>
                  --  Nothing to do here
                  null;
            end case;

         when A_Discrete_Subtype_Definition =>
            Append (Kinds, Create (Definition_Kinds'Image (A_Discrete_Subtype_Definition)));
            case Discrete_Range_Kind (Element) is
               when A_Discrete_Range_Attribute_Reference =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Discrete_Subtype_Definition > A_Discrete_Range_Attribute_Reference");
                  Append (Kinds, Create (Discrete_Range_Kinds'Image (A_Discrete_Range_Attribute_Reference)));

               when A_Discrete_Simple_Expression_Range =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Discrete_Subtype_Definition > A_Discrete_Simple_Expression_Range");
                  Append (Kinds, Create (Discrete_Range_Kinds'Image (A_Discrete_Simple_Expression_Range)));

               when A_Discrete_Subtype_Indication =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Discrete_Subtype_Definition > A_Discrete_Subtype_Indication");
                  Append (Kinds, Create (Discrete_Range_Kinds'Image (A_Discrete_Subtype_Indication)));

               when Not_A_Discrete_Range =>
                  --  Nothing to do here
                  null;
            end case;

         when A_Formal_Type_Definition =>
            Append (Kinds, Create (Definition_Kinds'Image (A_Formal_Type_Definition)));
            case Formal_Type_Kind (Element) is
               when A_Formal_Access_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Access_Type_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Access_Type_Definition)));

               when A_Formal_Constrained_Array_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Constrained_Array_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Constrained_Array_Definition)));

               when A_Formal_Decimal_Fixed_Point_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Decimal_Fixed_Point_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Decimal_Fixed_Point_Definition)));

               when A_Formal_Derived_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Derived_Type_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Derived_Type_Definition)));
                  Set_Trait_Kind;

               when A_Formal_Discrete_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Discrete_Type_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Discrete_Type_Definition)));

               when A_Formal_Floating_Point_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Floating_Point_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Floating_Point_Definition)));

               when A_Formal_Interface_Type_Definition =>
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Interface_Type_Definition)));
                  case Interface_Kind (Element) is
                     when A_Limited_Interface =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Interface_Type_Definition > A_Limited_Interface");
                        Append (Kinds, Create (Interface_Kinds'Image (A_Limited_Interface)));

                     when A_Protected_Interface =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Interface_Type_Definition > A_Protected_Interface");
                        Append (Kinds, Create (Interface_Kinds'Image (A_Protected_Interface)));

                     when A_Synchronized_Interface =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Interface_Type_Definition > A_Synchronized_Interface");
                        Append (Kinds, Create (Interface_Kinds'Image (A_Synchronized_Interface)));

                     when A_Task_Interface =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Interface_Type_Definition > A_Task_Interface");
                        Append (Kinds, Create (Interface_Kinds'Image (A_Task_Interface)));

                     when An_Ordinary_Interface =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Interface_Type_Definition > An_Ordinary_Interface");
                        Append (Kinds, Create (Interface_Kinds'Image (An_Ordinary_Interface)));

                     when Not_An_Interface =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Interface_Type_Definition > Not_An_Interface");
                        Append (Kinds, Create (Interface_Kinds'Image (Not_An_Interface)));
                  end case;

               when A_Formal_Modular_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Modular_Type_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Modular_Type_Definition)));

               when A_Formal_Ordinary_Fixed_Point_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Ordinary_Fixed_Point_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Ordinary_Fixed_Point_Definition)));

               when A_Formal_Private_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Private_Type_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Private_Type_Definition)));
                  Set_Trait_Kind;

               when A_Formal_Signed_Integer_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Signed_Integer_Type_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Signed_Integer_Type_Definition)));

               when A_Formal_Tagged_Private_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Tagged_Private_Type_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Tagged_Private_Type_Definition)));
                  Set_Trait_Kind;

               when A_Formal_Unconstrained_Array_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Formal_Type_Definition > A_Formal_Unconstrained_Array_Definition");
                  Append (Kinds, Create (Formal_Type_Kinds'Image (A_Formal_Unconstrained_Array_Definition)));

               when Not_A_Formal_Type_Definition =>
                  --  Nothing to do here
                  null;
            end case;

         when A_Known_Discriminant_Part =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Known_Discriminant_Part");
            Append (Kinds, Create (Definition_Kinds'Image (A_Known_Discriminant_Part)));

         when A_Null_Component =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Null_Component");
            Append (Kinds, Create (Definition_Kinds'Image (A_Null_Component)));

         when A_Null_Record_Definition =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Null_Record_Definition");
            Append (Kinds, Create (Definition_Kinds'Image (A_Null_Record_Definition)));

         when A_Private_Extension_Definition =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Private_Extension_Definition");
            Append (Kinds, Create (Definition_Kinds'Image (A_Private_Extension_Definition)));
            Set_Trait_Kind;

         when A_Private_Type_Definition =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Private_Type_Definition");
            Append (Kinds, Create (Definition_Kinds'Image (A_Private_Type_Definition)));
            Set_Trait_Kind;

         when A_Protected_Definition =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Protected_Definition");
            Append (Kinds, Create (Definition_Kinds'Image (A_Protected_Definition)));

         when A_Record_Definition =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Record_Definition");
            Append (Kinds, Create (Definition_Kinds'Image (A_Record_Definition)));

         when A_Subtype_Indication =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Subtype_Indication");
            Append (Kinds, Create (Definition_Kinds'Image (A_Subtype_Indication)));
            Set_Trait_Kind;

         when A_Tagged_Private_Type_Definition =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Tagged_Private_Type_Definition");
            Append (Kinds, Create (Definition_Kinds'Image (A_Tagged_Private_Type_Definition)));
            Set_Trait_Kind;

         when A_Task_Definition =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Task_Definition");
            Append (Kinds, Create (Definition_Kinds'Image (A_Task_Definition)));

         when A_Type_Definition =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition");
            Append (Kinds, Create (Definition_Kinds'Image (A_Type_Definition)));
            case Type_Kind (Element) is
               when A_Constrained_Array_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Constrained_Array_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (A_Constrained_Array_Definition)));

               when A_Decimal_Fixed_Point_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Decimal_Fixed_Point_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (A_Decimal_Fixed_Point_Definition)));

               when A_Derived_Record_Extension_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Derived_Record_Extension_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (A_Derived_Record_Extension_Definition)));
                  Set_Trait_Kind;
                  Set_Corresponding_Root_Type;

               when A_Derived_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Derived_Type_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (A_Derived_Type_Definition)));
                  Set_Trait_Kind;
                  Set_Corresponding_Root_Type;

               when A_Floating_Point_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Floating_Point_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (A_Floating_Point_Definition)));

               when A_Modular_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Modular_Type_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (A_Modular_Type_Definition)));

               when A_Record_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Record_Type_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (A_Record_Type_Definition)));
                  Set_Trait_Kind;

               when A_Root_Type_Definition =>
                  Append (Kinds, Create (Type_Kinds'Image (A_Root_Type_Definition)));
                  case Root_Type_Kind (Element) is
                     when A_Root_Integer_Definition =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Root_Type_Definition > A_Root_Integer_Definition");
                        Append (Kinds, Create (Root_Type_Kinds'Image (A_Root_Integer_Definition)));

                     when A_Root_Real_Definition =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Root_Type_Definition > A_Root_Real_Definition");
                        Append (Kinds, Create (Root_Type_Kinds'Image (A_Root_Real_Definition)));

                     when A_Universal_Fixed_Definition =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Root_Type_Definition > A_Universal_Fixed_Definition");
                        Append (Kinds, Create (Root_Type_Kinds'Image (A_Universal_Fixed_Definition)));

                     when A_Universal_Integer_Definition =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Root_Type_Definition > A_Universal_Integer_Definition");
                        Append (Kinds, Create (Root_Type_Kinds'Image (A_Universal_Integer_Definition)));

                     when A_Universal_Real_Definition =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Root_Type_Definition > A_Universal_Real_Definition");
                        Append (Kinds, Create (Root_Type_Kinds'Image (A_Universal_Real_Definition)));

                     when Not_A_Root_Type_Definition =>
                        --  Nothing to do here
                        null;
                  end case;

               when A_Signed_Integer_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Signed_Integer_Type_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (A_Signed_Integer_Type_Definition)));

               when A_Tagged_Record_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > A_Tagged_Record_Type_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (A_Tagged_Record_Type_Definition)));
                  Set_Trait_Kind;

               when An_Access_Type_Definition =>
                  Append (Kinds, Create (Type_Kinds'Image (An_Access_Type_Definition)));
                  case Access_Type_Kind (Element) is
                     when A_Pool_Specific_Access_To_Variable =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Access_Type_Definition > A_Pool_Specific_Access_To_Variable");
                        Append (Kinds, Create (Access_Type_Kinds'Image (A_Pool_Specific_Access_To_Variable)));

                     when An_Access_To_Constant =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Access_Type_Definition > An_Access_To_Constant");
                        Append (Kinds, Create (Access_Type_Kinds'Image (An_Access_To_Constant)));

                     when An_Access_To_Function =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Access_Type_Definition > An_Access_To_Function");
                        Append (Kinds, Create (Access_Type_Kinds'Image (An_Access_To_Function)));

                     when An_Access_To_Procedure =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Access_Type_Definition > An_Access_To_Procedure");
                        Append (Kinds, Create (Access_Type_Kinds'Image (An_Access_To_Procedure)));

                     when An_Access_To_Protected_Function =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Access_Type_Definition > An_Access_To_Protected_Function");
                        Append (Kinds, Create (Access_Type_Kinds'Image (An_Access_To_Protected_Function)));

                     when An_Access_To_Protected_Procedure =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Access_Type_Definition > An_Access_To_Protected_Procedure");
                        Append (Kinds, Create (Access_Type_Kinds'Image (An_Access_To_Protected_Procedure)));

                     when An_Access_To_Variable =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Access_Type_Definition > An_Access_To_Variable");
                        Append (Kinds, Create (Access_Type_Kinds'Image (An_Access_To_Variable)));

                     when Not_An_Access_Type_Definition =>
                        --  Nothing to do here
                        null;
                  end case;
                  Set_Trait_Kind;

               when An_Enumeration_Type_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Enumeration_Type_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (An_Enumeration_Type_Definition)));

               when An_Interface_Type_Definition =>
                  Append (Kinds, Create (Type_Kinds'Image (An_Interface_Type_Definition)));
                  case Interface_Kind (Element) is
                     when A_Limited_Interface =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Interface_Type_Definition > A_Limited_Interface");
                        Append (Kinds, Create (Interface_Kinds'Image (A_Limited_Interface)));

                     when A_Protected_Interface =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Interface_Type_Definition > A_Protected_Interface");
                        Append (Kinds, Create (Interface_Kinds'Image (A_Protected_Interface)));

                     when A_Synchronized_Interface =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Interface_Type_Definition > A_Synchronized_Interface");
                        Append (Kinds, Create (Interface_Kinds'Image (A_Synchronized_Interface)));

                     when A_Task_Interface =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Interface_Type_Definition > A_Task_Interface");
                        Append (Kinds, Create (Interface_Kinds'Image (A_Task_Interface)));

                     when An_Ordinary_Interface =>
                        Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Interface_Type_Definition > An_Ordinary_Interface");
                        Append (Kinds, Create (Interface_Kinds'Image (An_Ordinary_Interface)));

                     when Not_An_Interface =>
                        --  Nothing to do here
                        null;
                  end case;

               when An_Ordinary_Fixed_Point_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Ordinary_Fixed_Point_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (An_Ordinary_Fixed_Point_Definition)));

               when An_Unconstrained_Array_Definition =>
                  Log.Info ("[Pre_Procedure] A_Definition > A_Type_Definition > An_Unconstrained_Array_Definition");
                  Append (Kinds, Create (Type_Kinds'Image (An_Unconstrained_Array_Definition)));

               when Not_A_Type_Definition =>
                  --  Nothing to do here
                  null;
            end case;

         when A_Variant =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Variant");
            Append (Kinds, Create (Definition_Kinds'Image (A_Variant)));

         when A_Variant_Part =>
            Log.Info ("[Pre_Procedure] A_Definition > A_Variant_Part");
            Append (Kinds, Create (Definition_Kinds'Image (A_Variant_Part)));

         when An_Access_Definition =>
            Append (Kinds, Create (Definition_Kinds'Image (An_Access_Definition)));
            case Access_Definition_Kind (Element) is
               when An_Anonymous_Access_To_Constant =>
                  Log.Info ("[Pre_Procedure] A_Definition > An_Access_Definition > An_Anonymous_Access_To_Constant");
                  Append (Kinds, Create (Access_Definition_Kinds'Image (An_Anonymous_Access_To_Constant)));

               when An_Anonymous_Access_To_Function =>
                  Log.Info ("[Pre_Procedure] A_Definition > An_Access_Definition > An_Anonymous_Access_To_Function");
                  Append (Kinds, Create (Access_Definition_Kinds'Image (An_Anonymous_Access_To_Function)));

               when An_Anonymous_Access_To_Procedure =>
                  Log.Info ("[Pre_Procedure] A_Definition > An_Access_Definition > An_Anonymous_Access_To_Procedure");
                  Append (Kinds, Create (Access_Definition_Kinds'Image (An_Anonymous_Access_To_Procedure)));

               when An_Anonymous_Access_To_Protected_Function =>
                  Log.Info ("[Pre_Procedure] A_Definition > An_Access_Definition > An_Anonymous_Access_To_Protected_Function");
                  Append (Kinds, Create (Access_Definition_Kinds'Image (An_Anonymous_Access_To_Protected_Function)));

               when An_Anonymous_Access_To_Protected_Procedure =>
                  Log.Info ("[Pre_Procedure] A_Definition > An_Access_Definition > An_Anonymous_Access_To_Protected_Procedure");
                  Append (Kinds, Create (Access_Definition_Kinds'Image (An_Anonymous_Access_To_Protected_Procedure)));

               when An_Anonymous_Access_To_Variable =>
                  Log.Info ("[Pre_Procedure] A_Definition > An_Access_Definition > An_Anonymous_Access_To_Variable");
                  Append (Kinds, Create (Access_Definition_Kinds'Image (An_Anonymous_Access_To_Variable)));

               when Not_An_Access_Definition =>
                  --  Nothing to do here
                  null;
            end case;
            Set_Trait_Kind;

         when An_Aspect_Specification =>
            Log.Info ("[Pre_Procedure] A_Definition > An_Aspect_Specification");
            Append (Kinds, Create (Definition_Kinds'Image (An_Aspect_Specification)));

         when An_Others_Choice =>
            Log.Info ("[Pre_Procedure] A_Definition > An_Others_Choice");
            Append (Kinds, Create (Definition_Kinds'Image (An_Others_Choice)));

         when An_Unknown_Discriminant_Part =>
            Log.Info ("[Pre_Procedure] A_Definition > An_Unknown_Discriminant_Part");
            Append (Kinds, Create (Definition_Kinds'Image (An_Unknown_Discriminant_Part)));

         when Not_A_Definition =>
            --  Nothing to do here
            null;
         end case;

      when A_Path =>
         Append (Kinds, Create (Element_Kinds'Image (A_Path)));
         case Path_Kind (Element) is
         when A_Case_Expression_Path =>
            Log.Info ("[Pre_Procedure] A_Path > A_Case_Expression_Path");
            Append (Kinds, Create (Path_Kinds'Image (A_Case_Expression_Path)));

         when A_Case_Path =>
            Log.Info ("[Pre_Procedure] A_Path > A_Case_Path");
            Append (Kinds, Create (Path_Kinds'Image (A_Case_Path)));

         when A_Select_Path =>
            Log.Info ("[Pre_Procedure] A_Path > A_Select_Path");
            Append (Kinds, Create (Path_Kinds'Image (A_Select_Path)));

         when A_Then_Abort_Path =>
            Log.Info ("[Pre_Procedure] A_Path > A_Then_Abort_Path");
            Append (Kinds, Create (Path_Kinds'Image (A_Then_Abort_Path)));

         when An_Else_Expression_Path =>
            Log.Info ("[Pre_Procedure] A_Path > An_Else_Expression_Path");
            Append (Kinds, Create (Path_Kinds'Image (An_Else_Expression_Path)));

         when An_Else_Path =>
            Log.Info ("[Pre_Procedure] A_Path > An_Else_Path");
            Append (Kinds, Create (Path_Kinds'Image (An_Else_Path)));

         when An_Elsif_Expression_Path =>
            Log.Info ("[Pre_Procedure] A_Path > An_Elsif_Expression_Path");
            Append (Kinds, Create (Path_Kinds'Image (An_Elsif_Expression_Path)));

         when An_Elsif_Path =>
            Log.Info ("[Pre_Procedure] A_Path > An_Elsif_Path");
            Append (Kinds, Create (Path_Kinds'Image (An_Elsif_Path)));

         when An_If_Expression_Path =>
            Log.Info ("[Pre_Procedure] A_Path > An_If_Expression_Path");
            Append (Kinds, Create (Path_Kinds'Image (An_If_Expression_Path)));

         when An_If_Path =>
            Log.Info ("[Pre_Procedure] A_Path > An_If_Path");
            Append (Kinds, Create (Path_Kinds'Image (An_If_Path)));

         when An_Or_Path =>
            Log.Info ("[Pre_Procedure] A_Path > An_Or_Path");
            Append (Kinds, Create (Path_Kinds'Image (An_Or_Path)));

         when Not_A_Path =>
            --  Nothing to do here
            null;
         end case;

      when A_Pragma =>
         Append (Kinds, Create (Element_Kinds'Image (A_Pragma)));
         case Pragma_Kind (Element) is
         when A_Controlled_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Controlled_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Controlled_Pragma)));

         when A_Convention_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Convention_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Convention_Pragma)));

         when A_CPU_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_CPU_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_CPU_Pragma)));

         when A_Default_Storage_Pool_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Default_Storage_Pool_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Default_Storage_Pool_Pragma)));

         when A_Detect_Blocking_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Detect_Blocking_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Detect_Blocking_Pragma)));

         when A_Discard_Names_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Discard_Names_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Discard_Names_Pragma)));

         when A_Dispatching_Domain_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Dispatching_Domain_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Dispatching_Domain_Pragma)));

         when A_Independent_Components_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Independent_Components_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Independent_Components_Pragma)));

         when A_Linker_Options_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Linker_Options_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Linker_Options_Pragma)));

         when A_List_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_List_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_List_Pragma)));

         when A_Locking_Policy_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Locking_Policy_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Locking_Policy_Pragma)));

         when A_No_Return_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_No_Return_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_No_Return_Pragma)));

         when A_Normalize_Scalars_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Normalize_Scalars_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Normalize_Scalars_Pragma)));

         when A_Pack_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Pack_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Pack_Pragma)));

         when A_Page_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Page_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Page_Pragma)));

         when A_Partition_Elaboration_Policy_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Partition_Elaboration_Policy_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Partition_Elaboration_Policy_Pragma)));

         when A_Preelaborable_Initialization_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Preelaborable_Initialization_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Preelaborable_Initialization_Pragma)));

         when A_Preelaborate_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Preelaborate_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Preelaborate_Pragma)));

         when A_Priority_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Priority_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Priority_Pragma)));

         when A_Priority_Specific_Dispatching_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Priority_Specific_Dispatching_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Priority_Specific_Dispatching_Pragma)));

         when A_Profile_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Profile_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Profile_Pragma)));

         when A_Pure_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Pure_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Pure_Pragma)));

         when A_Queuing_Policy_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Queuing_Policy_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Queuing_Policy_Pragma)));

         when A_Relative_Deadline_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Relative_Deadline_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Relative_Deadline_Pragma)));

         when A_Remote_Call_Interface_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Remote_Call_Interface_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Remote_Call_Interface_Pragma)));

         when A_Remote_Types_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Remote_Types_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Remote_Types_Pragma)));

         when A_Restrictions_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Restrictions_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Restrictions_Pragma)));

         when A_Reviewable_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Reviewable_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Reviewable_Pragma)));

         when A_Shared_Passive_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Shared_Passive_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Shared_Passive_Pragma)));

         when A_Storage_Size_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Storage_Size_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Storage_Size_Pragma)));

         when A_Suppress_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Suppress_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Suppress_Pragma)));

         when A_Task_Dispatching_Policy_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Task_Dispatching_Policy_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Task_Dispatching_Policy_Pragma)));

         when A_Volatile_Components_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Volatile_Components_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Volatile_Components_Pragma)));

         when A_Volatile_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > A_Volatile_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (A_Volatile_Pragma)));

         when An_All_Calls_Remote_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_All_Calls_Remote_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_All_Calls_Remote_Pragma)));

         when An_Assert_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Assert_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Assert_Pragma)));

         when An_Assertion_Policy_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Assertion_Policy_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Assertion_Policy_Pragma)));

         when An_Asynchronous_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Asynchronous_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Asynchronous_Pragma)));

         when An_Atomic_Components_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Atomic_Components_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Atomic_Components_Pragma)));

         when An_Atomic_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Atomic_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Atomic_Pragma)));

         when An_Attach_Handler_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Attach_Handler_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Attach_Handler_Pragma)));

         when An_Elaborate_All_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Elaborate_All_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Elaborate_All_Pragma)));

         when An_Elaborate_Body_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Elaborate_Body_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Elaborate_Body_Pragma)));

         when An_Elaborate_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Elaborate_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Elaborate_Pragma)));

         when An_Export_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Export_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Export_Pragma)));

         when An_Implementation_Defined_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Implementation_Defined_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Implementation_Defined_Pragma)));

         when An_Import_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Import_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Import_Pragma)));

         when An_Independent_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Independent_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Independent_Pragma)));

         when An_Inline_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Inline_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Inline_Pragma)));

         when An_Inspection_Point_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Inspection_Point_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Inspection_Point_Pragma)));

         when An_Interrupt_Handler_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Interrupt_Handler_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Interrupt_Handler_Pragma)));

         when An_Interrupt_Priority_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Interrupt_Priority_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Interrupt_Priority_Pragma)));

         when An_Optimize_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Optimize_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Optimize_Pragma)));

         when An_Unchecked_Union_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Unchecked_Union_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Unchecked_Union_Pragma)));

         when An_Unknown_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Unknown_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Unknown_Pragma)));

         when An_Unsuppress_Pragma =>
            Log.Info ("[Pre_Procedure] A_Pragma > An_Unsuppress_Pragma");
            Append (Kinds, Create (Pragma_Kinds'Image (An_Unsuppress_Pragma)));

         when Not_A_Pragma =>
            --  Nothing to do here
            null;
         end case;

      when A_Statement =>
         Append (Kinds, Create (Element_Kinds'Image (A_Statement)));
         case Statement_Kind (Element) is
         when A_Block_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Block_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Block_Statement)));

         when A_Case_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Case_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Case_Statement)));

         when A_Code_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Code_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Code_Statement)));

         when A_Conditional_Entry_Call_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Conditional_Entry_Call_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Conditional_Entry_Call_Statement)));

         when A_Delay_Relative_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Delay_Relative_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Delay_Relative_Statement)));

         when A_Delay_Until_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Delay_Until_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Delay_Until_Statement)));

         when A_For_Loop_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_For_Loop_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_For_Loop_Statement)));

         when A_Goto_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Goto_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Goto_Statement)));

         when A_Loop_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Loop_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Loop_Statement)));

         when A_Null_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Null_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Null_Statement)));

         when A_Procedure_Call_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Procedure_Call_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Procedure_Call_Statement)));

         when A_Raise_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Raise_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Raise_Statement)));

         when A_Requeue_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Requeue_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Requeue_Statement)));

         when A_Requeue_Statement_With_Abort =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Requeue_Statement_With_Abort");
            Append (Kinds, Create (Statement_Kinds'Image (A_Requeue_Statement_With_Abort)));

         when A_Return_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Return_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Return_Statement)));

         when A_Selective_Accept_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Selective_Accept_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Selective_Accept_Statement)));

         when A_Terminate_Alternative_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Terminate_Alternative_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Terminate_Alternative_Statement)));

         when A_Timed_Entry_Call_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_Timed_Entry_Call_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_Timed_Entry_Call_Statement)));

         when A_While_Loop_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > A_While_Loop_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (A_While_Loop_Statement)));

         when An_Abort_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > An_Abort_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (An_Abort_Statement)));

         when An_Accept_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > An_Accept_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (An_Accept_Statement)));

         when An_Assignment_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > An_Assignment_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (An_Assignment_Statement)));

         when An_Asynchronous_Select_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > An_Asynchronous_Select_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (An_Asynchronous_Select_Statement)));

         when An_Entry_Call_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > An_Entry_Call_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (An_Entry_Call_Statement)));

         when An_Exit_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > An_Exit_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (An_Exit_Statement)));
            -- TODO: add information related to the exited loop

         when An_Extended_Return_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > An_Extended_Return_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (An_Extended_Return_Statement)));
            declare
               Return_Obj_Decl : constant Asis.Declaration := Asis.Statements.Return_Object_Declaration (Element);
            begin
               if Element_Kind (Return_Obj_Decl) = A_Declaration then
                  Set_Trait_Kind;
               end if;
            end;

         when An_If_Statement =>
            Log.Info ("[Pre_Procedure] A_Statement > An_If_Statement");
            Append (Kinds, Create (Statement_Kinds'Image (An_If_Statement)));

         when Not_A_Statement =>
            --  Nothing to do here
            null;
         end case;

      when An_Association =>
         Append (Kinds, Create (Element_Kinds'Image (An_Association)));
         case Association_Kind (Element) is
         when A_Discriminant_Association =>
            Log.Info ("[Pre_Procedure] An_Association > A_Discriminant_Association");
            Append (Kinds, Create (Association_Kinds'Image (A_Discriminant_Association)));

         when A_Generic_Association =>
            Log.Info ("[Pre_Procedure] An_Association > A_Generic_Association");
            Append (Kinds, Create (Association_Kinds'Image (A_Generic_Association)));

         when A_Parameter_Association =>
            Log.Info ("[Pre_Procedure] An_Association > A_Parameter_Association");
            Append (Kinds, Create (Association_Kinds'Image (A_Parameter_Association)));

            -- TODO: manage other callable things
            Process_Parameter_Association : declare
               use all type Thick_Queries.Call_Kind;
            begin
               if Statement_Kind (Parent) in A_Procedure_Call_Statement | An_Entry_Call_Statement
                 or else (Expression_Kind (Parent) in A_Function_Call
                     and then Thick_Queries.Corresponding_Call_Description (Parent).Kind = A_Regular_Call)
               then
                  Set_Parameter_Info : declare
                     Current_Formal_Name : constant Asis.Element :=
                       (if State.Parameters_Information.Normalized then
                           Asis.Expressions.Formal_Parameter (Element)
                        else
                           Thick_Queries.Formal_Name (Element)
                       );
                  begin
                     --  In case of dynamic call ('Class parameter), the Current_Formal_Name is empty since
                     --  We cannot know to which sub-program it realy point.
                     --  TODO: add a special relationship or property of ref to spec, to point to the
                     --  original sub-program (parent, where 'Class is declared)
                     if Element_Kind (Current_Formal_Name) /= Not_An_Element then
                        declare
                           Json_Param_Spec                       : constant JSON_Value := Create_Object;
                           Corresponding_Parameter_Specification : constant Asis.Declaration :=
                             Enclosing_Element (Current_Formal_Name);
                           Current_Actual_Expression             : constant Asis.Element :=
                             Asis.Expressions.Actual_Parameter (Element);
                           Corresponding_User_Parameter          : constant JSON_Value   := Clone (Json_Parent);
                           Corresponding_Formal_Name             : constant JSON_Value   := Create_Object;

                           Elts : Vector renames State.Parameters_Information.Elements;

                           Index                    : Positive   := 1;
                           Found                    : Boolean    := False;
                           Enclosing_Relation_Props : JSON_Array;
                        begin
                           -- Set Corresponding_Parameter_Specification
                           Json_Param_Spec.Set_Field ("location", Get_Fixed_Location (Corresponding_Parameter_Specification));
                           Json_Param_Spec.Set_Field ("kind", Declaration_Kinds'Image (A_Parameter_Specification));
                           Json_Param_Spec.Set_Field ("node_id", Get_Node_Id (Json_Param_Spec));
                           Json_Element.Set_Field (Export_Utils.CORRESPONDING_PARAMETER_SPECIFICATION'Image, Json_Param_Spec);

                           -- Set node info (is_default_parameter, is_named_parameter_association)
                           if not Is_Empty (Elts) then
                              Json_Element.Set_Field ("is_default_parameter", False);

                              if not Elts.First_Element.Is_Named_Parameter_Association then
                                 -- Pop the first element
                                 Elts.Delete_First;
                                 Json_Element.Set_Field ("is_named_parameter_association", False);

                                 if State.Parameters_Information.Normalized then
                                    Json_Location.Set_Field ("column", State.Parameters_Information.Current_Primitive_Index * (-1));
                                    Add_Property (Arr   => Enclosing_Relation_Props,
                                                  Key   => "index",
                                                  Value => State.Parameters_Information.Current_Primitive_Index);
                                    Json_Parent.Set_Field ("properties", Enclosing_Relation_Props);
                                 end if;
                                 State.Parameters_Information.Current_Primitive_Index := State.Parameters_Information.Current_Primitive_Index + 1;
                              else
                                 -- FIXME: the commented below code can maybe create an issue
                                 -- Commented to test set column of current location when parameters are normalized
                                 --                          State.Parameters_Information.Current_Primitive_Index := 0;

                                 -- Find element where Formal = Current_Formal_Name
                                 Find_Good_Parameter : for Elt : Parameter_Info of Elts loop
                                    if Elt.Is_Named_Parameter_Association
                                      and Thick_Queries.Full_Name_Image (Elt.Formal_Name) = Thick_Queries.Full_Name_Image (Current_Formal_Name)
                                    then
                                       -- Found named parameter association
                                       Found := True;
                                       exit Find_Good_Parameter;
                                    end if;
                                    Index := Index + 1;
                                 end loop Find_Good_Parameter;

                                 if not Found then
                                    -- Default parameter
                                    Json_Element.Set_Field ("is_default_parameter", True);
                                    Json_Element.Set_Field ("is_named_parameter_association", False);
                                 else
                                    Elts.Delete (Index);
                                    Json_Element.Set_Field ("is_named_parameter_association", True);
                                 end if;

                                 if State.Parameters_Information.Normalized then
                                    Json_Location.Set_Field ("column", State.Parameters_Information.Current_Primitive_Index * (-1));
                                    Add_Property (Arr   => Enclosing_Relation_Props,
                                                  Key   => "index",
                                                  Value => State.Parameters_Information.Current_Primitive_Index);
                                    Json_Parent.Set_Field ("properties", Enclosing_Relation_Props);
                                    State.Parameters_Information.Current_Primitive_Index := State.Parameters_Information.Current_Primitive_Index + 1;
                                 end if;
                              end if;
                           else
                              --  Default parameter
                              Json_Element.Set_Field ("is_default_parameter", True);
                              Json_Element.Set_Field ("is_named_parameter_association", False);

                              if State.Parameters_Information.Normalized then
                                 Json_Location.Set_Field ("column", State.Parameters_Information.Current_Primitive_Index * (-1));
                                 Add_Property (Arr   => Enclosing_Relation_Props,
                                               Key   => "index",
                                               Value => State.Parameters_Information.Current_Primitive_Index);
                                 Json_Parent.Set_Field ("properties", Enclosing_Relation_Props);
                                 State.Parameters_Information.Current_Primitive_Index := State.Parameters_Information.Current_Primitive_Index + 1;
                              end if;
                           end if;

                           -- Set Corresponding_Formal_Name
                           Corresponding_Formal_Name.Set_Field ("location", Get_Fixed_Location (Current_Formal_Name));
                           Corresponding_Formal_Name.Set_Field ("kind", Get_Kind_Image (Current_Formal_Name));
                           Corresponding_Formal_Name.Set_Field ("node_id", Get_Node_Id (Corresponding_Formal_Name));
                           Json_Element.Set_Field (Export_Utils.CORRESPONDING_FORMAL_NAME'Image, Corresponding_Formal_Name);

                           Set_Corresponding_Actual_Parameter : declare
                              Corresponding_Actual_Parameter : constant JSON_Value := Create_Object;
                              Current_Actual_Expression_JSON : constant JSON_Value := Create_Object;
                           begin
                              Corresponding_Actual_Parameter.Set_Field  ("location", Clone (Json_Element.Get ("location")));
                              Corresponding_Actual_Parameter.Set_Field ("kind", Get_Kind_Image (Element));
                              -- The oreder of this relation is inverted because we delayed this relation.
                              -- This relation will be added when we have met the origin node, that is a child of the current.
                              Corresponding_Actual_Parameter.Set_Field ("reverse", True);

                              Current_Actual_Expression_JSON.Set_Field ("location", Get_Fixed_Location (Current_Actual_Expression));
                              Current_Actual_Expression_JSON.Set_Field ("kind", Get_Kind_Image (Current_Actual_Expression));
                              Current_Actual_Expression_JSON.Set_Field ("node_id", Get_Node_Id (Current_Actual_Expression_JSON));

                              if State.Parameters_Information.Normalized then
                                 Json_Element.Set_Field
                                   (Export_Utils.CORRESPONDING_ACTUAL_PARAMETER'Image,
                                    Current_Actual_Expression_JSON);
                              else
                                 State.Parameters_Information.Association :=
                                   (Corresponding_Actual_Parameter, Current_Actual_Expression_JSON);
                              end if;
                           end Set_Corresponding_Actual_Parameter;
                        end;
                     end if;
                  end Set_Parameter_Info;
               end if;

            end Process_Parameter_Association;
         when A_Pragma_Argument_Association =>
            Log.Info ("[Pre_Procedure] An_Association > A_Pragma_Argument_Association");
            Append (Kinds, Create (Association_Kinds'Image (A_Pragma_Argument_Association)));

         when A_Record_Component_Association =>
            Log.Info ("[Pre_Procedure] An_Association > A_Record_Component_Association");
            Append (Kinds, Create (Association_Kinds'Image (A_Record_Component_Association)));

         when An_Array_Component_Association =>
            Log.Info ("[Pre_Procedure] An_Association > An_Array_Component_Association");
            Append (Kinds, Create (Association_Kinds'Image (An_Array_Component_Association)));

         when Not_An_Association =>
            --  Nothing to do here
            null;
         end case;

      when An_Exception_Handler =>
         --  There is no "Exception_Handler_Kind".
         --  TODO
         null;

      when An_Expression =>
         Append (Kinds, Create (Element_Kinds'Image (An_Expression)));
         case Expression_Kind (Element) is
         when A_Box_Expression =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Box_Expression");
            Append (Kinds, Create (Expression_Kinds'Image (A_Box_Expression)));

         when A_Case_Expression =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Case_Expression");
            Append (Kinds, Create (Expression_Kinds'Image (A_Case_Expression)));

         when A_Character_Literal =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Character_Literal");
            Append (Kinds, Create (Expression_Kinds'Image (A_Character_Literal)));
            Set_Corresponding_Name_Definition;

         when A_For_All_Quantified_Expression =>
            Log.Info ("[Pre_Procedure] An_Expression > A_For_All_Quantified_Expression");
            Append (Kinds, Create (Expression_Kinds'Image (A_For_All_Quantified_Expression)));

         when A_For_Some_Quantified_Expression =>
            Log.Info ("[Pre_Procedure] An_Expression > A_For_Some_Quantified_Expression");
            Append (Kinds, Create (Expression_Kinds'Image (A_For_Some_Quantified_Expression)));

         when A_Function_Call =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Function_Call");
            Append (Kinds, Create (Expression_Kinds'Image (A_Function_Call)));

         when A_Named_Array_Aggregate =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Named_Array_Aggregate");
            Append (Kinds, Create (Expression_Kinds'Image (A_Named_Array_Aggregate)));

         when A_Not_In_Membership_Test =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Not_In_Membership_Test");
            Append (Kinds, Create (Expression_Kinds'Image (A_Not_In_Membership_Test)));

         when A_Null_Literal =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Null_Literal");
            Append (Kinds, Create (Expression_Kinds'Image (A_Null_Literal)));

         when A_Parenthesized_Expression =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Parenthesized_Expression");
            Append (Kinds, Create (Expression_Kinds'Image (A_Parenthesized_Expression)));

         when A_Positional_Array_Aggregate =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Positional_Array_Aggregate");
            Append (Kinds, Create (Expression_Kinds'Image (A_Positional_Array_Aggregate)));

         when A_Qualified_Expression =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Qualified_Expression");
            Append (Kinds, Create (Expression_Kinds'Image (A_Qualified_Expression)));

         when A_Raise_Expression =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Raise_Expression");
            Append (Kinds, Create (Expression_Kinds'Image (A_Raise_Expression)));

         when A_Real_Literal =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Real_Literal");
            Append (Kinds, Create (Expression_Kinds'Image (A_Real_Literal)));

         when A_Record_Aggregate =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Record_Aggregate");
            Append (Kinds, Create (Expression_Kinds'Image (A_Record_Aggregate)));

         when A_Selected_Component =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Selected_Component");
            Append (Kinds, Create (Expression_Kinds'Image (A_Selected_Component)));

         when A_Slice =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Slice");
            Append (Kinds, Create (Expression_Kinds'Image (A_Slice)));

         when A_String_Literal =>
            Log.Info ("[Pre_Procedure] An_Expression > A_String_Literal");
            Append (Kinds, Create (Expression_Kinds'Image (A_String_Literal)));

         when A_Type_Conversion =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Type_Conversion");
            Append (Kinds, Create (Expression_Kinds'Image (A_Type_Conversion)));

         when An_Allocation_From_Qualified_Expression =>
            Log.Info ("[Pre_Procedure] An_Expression > An_Allocation_From_Qualified_Expression");
            Append (Kinds, Create (Expression_Kinds'Image (An_Allocation_From_Qualified_Expression)));

         when An_Allocation_From_Subtype =>
            Log.Info ("[Pre_Procedure] An_Expression > An_Allocation_From_Subtype");
            Append (Kinds, Create (Expression_Kinds'Image (An_Allocation_From_Subtype)));

         when An_And_Then_Short_Circuit =>
            Log.Info ("[Pre_Procedure] An_Expression > An_And_Then_Short_Circuit");
            Append (Kinds, Create (Expression_Kinds'Image (An_And_Then_Short_Circuit)));

         when An_Attribute_Reference =>
            Append (Kinds, Create (Expression_Kinds'Image (An_Attribute_Reference)));
            case Attribute_Kind (Element) is
               when A_Base_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Base_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Base_Attribute)));

               when A_Bit_Order_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Bit_Order_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Bit_Order_Attribute)));

               when A_Body_Version_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Body_Version_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Body_Version_Attribute)));

               when A_Callable_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Callable_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Callable_Attribute)));

               when A_Caller_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Caller_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Caller_Attribute)));

               when A_Ceiling_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Ceiling_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Ceiling_Attribute)));

               when A_Class_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Class_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Class_Attribute)));

               when A_Component_Size_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Component_Size_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Component_Size_Attribute)));

               when A_Compose_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Compose_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Compose_Attribute)));

               when A_Constrained_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Constrained_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Constrained_Attribute)));

               when A_Copy_Sign_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Copy_Sign_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Copy_Sign_Attribute)));

               when A_Count_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Count_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Count_Attribute)));

               when A_Definite_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Definite_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Definite_Attribute)));

               when A_Delta_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Delta_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Delta_Attribute)));

               when A_Denorm_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Denorm_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Denorm_Attribute)));

               when A_Digits_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Digits_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Digits_Attribute)));

               when A_First_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_First_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_First_Attribute)));

               when A_First_Bit_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_First_Bit_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_First_Bit_Attribute)));

               when A_Floor_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Floor_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Floor_Attribute)));

               when A_Fore_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Fore_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Fore_Attribute)));

               when A_Fraction_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Fraction_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Fraction_Attribute)));

               when A_Last_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Last_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Last_Attribute)));

               when A_Last_Bit_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Last_Bit_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Last_Bit_Attribute)));

               when A_Leading_Part_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Leading_Part_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Leading_Part_Attribute)));

               when A_Length_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Length_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Length_Attribute)));

               when A_Machine_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Machine_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Machine_Attribute)));

               when A_Machine_Emax_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Machine_Emax_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Machine_Emax_Attribute)));

               when A_Machine_Emin_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Machine_Emin_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Machine_Emin_Attribute)));

               when A_Machine_Mantissa_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Machine_Mantissa_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Machine_Mantissa_Attribute)));

               when A_Machine_Overflows_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Machine_Overflows_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Machine_Overflows_Attribute)));

               when A_Machine_Radix_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Machine_Radix_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Machine_Radix_Attribute)));

               when A_Machine_Rounding_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Machine_Rounding_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Machine_Rounding_Attribute)));

               when A_Machine_Rounds_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Machine_Rounds_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Machine_Rounds_Attribute)));

               when A_Max_Alignment_For_Allocation_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Max_Alignment_For_Allocation_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Max_Alignment_For_Allocation_Attribute)));

               when A_Max_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Max_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Max_Attribute)));

               when A_Max_Size_In_Storage_Elements_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Max_Size_In_Storage_Elements_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Max_Size_In_Storage_Elements_Attribute)));

               when A_Min_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Min_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Min_Attribute)));

               when A_Mod_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Mod_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Mod_Attribute)));

               when A_Model_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Model_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Model_Attribute)));

               when A_Model_Emin_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Model_Emin_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Model_Emin_Attribute)));

               when A_Model_Epsilon_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Model_Epsilon_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Model_Epsilon_Attribute)));

               when A_Model_Mantissa_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Model_Mantissa_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Model_Mantissa_Attribute)));

               when A_Model_Small_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Model_Small_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Model_Small_Attribute)));

               when A_Modulus_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Modulus_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Modulus_Attribute)));

               when A_Partition_ID_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Partition_ID_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Partition_ID_Attribute)));

               when A_Pos_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Pos_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Pos_Attribute)));

               when A_Position_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Position_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Position_Attribute)));

               when A_Pred_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Pred_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Pred_Attribute)));

               when A_Priority_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Priority_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Priority_Attribute)));

               when A_Range_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Range_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Range_Attribute)));

               when A_Read_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Read_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Read_Attribute)));

               when A_Remainder_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Remainder_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Remainder_Attribute)));

               when A_Round_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Round_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Round_Attribute)));

               when A_Rounding_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Rounding_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Rounding_Attribute)));

               when A_Safe_First_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Safe_First_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Safe_First_Attribute)));

               when A_Safe_Last_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Safe_Last_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Safe_Last_Attribute)));

               when A_Scale_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Scale_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Scale_Attribute)));

               when A_Scaling_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Scaling_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Scaling_Attribute)));

               when A_Signed_Zeros_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Signed_Zeros_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Signed_Zeros_Attribute)));

               when A_Size_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Size_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Size_Attribute)));

               when A_Small_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Small_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Small_Attribute)));

               when A_Storage_Pool_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Storage_Pool_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Storage_Pool_Attribute)));

               when A_Storage_Size_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Storage_Size_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Storage_Size_Attribute)));

               when A_Stream_Size_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Stream_Size_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Stream_Size_Attribute)));

               when A_Succ_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Succ_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Succ_Attribute)));

               when A_Tag_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Tag_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Tag_Attribute)));

               when A_Terminated_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Terminated_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Terminated_Attribute)));

               when A_Truncation_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Truncation_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Truncation_Attribute)));

               when A_Val_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Val_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Val_Attribute)));

               when A_Valid_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Valid_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Valid_Attribute)));

               when A_Value_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Value_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Value_Attribute)));

               when A_Version_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Version_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Version_Attribute)));

               when A_Wide_Image_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Wide_Image_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Wide_Image_Attribute)));

               when A_Wide_Value_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Wide_Value_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Wide_Value_Attribute)));

               when A_Wide_Wide_Image_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Wide_Wide_Image_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Wide_Wide_Image_Attribute)));

               when A_Wide_Wide_Value_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Wide_Wide_Value_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Wide_Wide_Value_Attribute)));

               when A_Wide_Wide_Width_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Wide_Wide_Width_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Wide_Wide_Width_Attribute)));

               when A_Wide_Width_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Wide_Width_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Wide_Width_Attribute)));

               when A_Width_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Width_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Width_Attribute)));

               when A_Write_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > A_Write_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (A_Write_Attribute)));

               when An_Access_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Access_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Access_Attribute)));

               when An_Address_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Address_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Address_Attribute)));

               when An_Adjacent_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Adjacent_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Adjacent_Attribute)));

               when An_Aft_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Aft_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Aft_Attribute)));

               when An_Alignment_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Alignment_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Alignment_Attribute)));

               when An_Exponent_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Exponent_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Exponent_Attribute)));

               when An_External_Tag_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_External_Tag_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_External_Tag_Attribute)));

               when An_Identity_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Identity_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Identity_Attribute)));

               when An_Image_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Image_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Image_Attribute)));

               when An_Implementation_Defined_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Implementation_Defined_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Implementation_Defined_Attribute)));

               when An_Input_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Input_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Input_Attribute)));

               when An_Output_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Output_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Output_Attribute)));

               when An_Overlaps_Storage_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Overlaps_Storage_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Overlaps_Storage_Attribute)));

               when An_Unbiased_Rounding_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Unbiased_Rounding_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Unbiased_Rounding_Attribute)));

               when An_Unchecked_Access_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Unchecked_Access_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Unchecked_Access_Attribute)));

               when An_Unknown_Attribute =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Attribute_Reference > An_Unknown_Attribute");
                  Append (Kinds, Create (Attribute_Kinds'Image (An_Unknown_Attribute)));

               when Not_An_Attribute =>
                  --  Nothing to do here
                  null;
            end case;

         when An_Enumeration_Literal =>
            Log.Info ("[Pre_Procedure] An_Expression > An_Enumeration_Literal");
            Append (Kinds, Create (Expression_Kinds'Image (An_Enumeration_Literal)));
            Set_Corresponding_Name_Definition;

         when An_Explicit_Dereference =>
            Log.Info ("[Pre_Procedure] An_Expression > An_Explicit_Dereference");
            Append (Kinds, Create (Expression_Kinds'Image (An_Explicit_Dereference)));

         when An_Extension_Aggregate =>
            Log.Info ("[Pre_Procedure] An_Expression > An_Extension_Aggregate");
            Append (Kinds, Create (Expression_Kinds'Image (An_Extension_Aggregate)));

         when An_Identifier =>
            Log.Info ("[Pre_Procedure] An_Expression > An_Identifier");
            Append (Kinds, Create (Expression_Kinds'Image (An_Identifier)));
            Set_Corresponding_Name_Definition;

            if not Is_Nil (State.Corresponding_Assignment_Statement) then
               declare
                  CND    : Asis.Element := Nil_Element;
                  Parent : constant Asis.Element := Enclosing_Element (Element);
               begin
                  if Expression_Kind (Parent) /= An_Attribute_Reference then
                     begin
                        CND := Asis.Expressions.Corresponding_Name_Definition (Element);
                     exception
                        when E : Asis.Exceptions.Asis_Inappropriate_Element =>
                           --  In some cases, there is no declaration
                           --  Take a look at the end of the Asis.Expressions.Corresponding_Name_Definition
                           return;
                     end;

                     if Element_Kind (CND) /= Not_An_Element then
                        declare
                           use Asis.Expressions;
                           use Thick_Queries;
                           Encl : constant Asis.Element := Enclosing_Element (CND);
                        begin
                           if Declaration_Kind (Encl) = A_Variable_Declaration and then
                             -- And is not an implicit dereference
                             (Expression_Kind (Parent) /= A_Selected_Component or else not Is_Access_Expression (Prefix (Parent)))
                           then
                              declare
                                 Json_Elt : constant JSON_Value := Create_Object;
                              begin
                                 Json_Elt.Set_Field ("location", Get_Fixed_Location (State.Corresponding_Assignment_Statement));
                                 Json_Elt.Set_Field ("kind", Get_Kind_Image (State.Corresponding_Assignment_Statement));
                                 Json_Elt.Set_Field ("node_id", Get_Node_Id (Json_Elt));
                                 Json_Element.Set_Field (Export_Utils.CORRESPONDING_ASSIGNATION'Image, Json_Elt);
                              end;
                              State.Corresponding_Assignment_Statement := Asis.Nil_Element;
                           end if;
                        end;
                     end if;
                  end if;
               end;
            end if;

         when An_If_Expression =>
            Log.Info ("[Pre_Procedure] An_Expression > An_If_Expression");
            Append (Kinds, Create (Expression_Kinds'Image (An_If_Expression)));

         when An_In_Membership_Test =>
            Log.Info ("[Pre_Procedure] An_Expression > An_In_Membership_Test");
            Append (Kinds, Create (Expression_Kinds'Image (An_In_Membership_Test)));

         when An_Indexed_Component =>
            Log.Info ("[Pre_Procedure] An_Expression > An_Indexed_Component");
            Append (Kinds, Create (Expression_Kinds'Image (An_Indexed_Component)));

         when An_Integer_Literal =>
            Log.Info ("[Pre_Procedure] An_Expression > An_Integer_Literal");
            Append (Kinds, Create (Expression_Kinds'Image (An_Integer_Literal)));

         when An_Operator_Symbol =>
            Append (Kinds, Create (Expression_Kinds'Image (An_Operator_Symbol)));
            case Operator_Kind (Element) is
               when A_Concatenate_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Concatenate_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Concatenate_Operator)));

               when A_Divide_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Divide_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Divide_Operator)));

               when A_Greater_Than_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Greater_Than_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Greater_Than_Operator)));

               when A_Greater_Than_Or_Equal_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Greater_Than_Or_Equal_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Greater_Than_Or_Equal_Operator)));

               when A_Less_Than_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Less_Than_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Less_Than_Operator)));

               when A_Less_Than_Or_Equal_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Less_Than_Or_Equal_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Less_Than_Or_Equal_Operator)));

               when A_Minus_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Minus_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Minus_Operator)));

               when A_Mod_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Mod_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Mod_Operator)));

               when A_Multiply_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Multiply_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Multiply_Operator)));

               when A_Not_Equal_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Not_Equal_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Not_Equal_Operator)));

               when A_Not_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Not_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Not_Operator)));

               when A_Plus_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Plus_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Plus_Operator)));

               when A_Rem_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Rem_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Rem_Operator)));

               when A_Unary_Minus_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Unary_Minus_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Unary_Minus_Operator)));

               when A_Unary_Plus_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > A_Unary_Plus_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (A_Unary_Plus_Operator)));

               when An_Abs_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > An_Abs_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_Abs_Operator)));

               when An_And_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > An_And_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_And_Operator)));

               when An_Equal_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > An_Equal_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_Equal_Operator)));

               when An_Exponentiate_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > An_Exponentiate_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_Exponentiate_Operator)));

               when An_Or_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > An_Or_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_Or_Operator)));

               when An_Xor_Operator =>
                  Log.Info ("[Pre_Procedure] An_Expression > An_Operator_Symbol > An_Xor_Operator");
                  Append (Kinds, Create (Operator_Kinds'Image (An_Xor_Operator)));

               when Not_An_Operator =>
                  --  Nothing to do here
                  null;
            end case;
            Set_Corresponding_Name_Definition;

         when An_Or_Else_Short_Circuit =>
            Log.Info ("[Pre_Procedure] An_Expression > An_Or_Else_Short_Circuit");
            Append (Kinds, Create (Expression_Kinds'Image (An_Or_Else_Short_Circuit)));

         when A_Target_Name =>
            Log.Info ("[Pre_Procedure] An_Expression > A_Target_Name");
            Append (Kinds, Create (Expression_Kinds'Image (A_Target_Name)));

         when Not_An_Expression =>
            --  Nothing to do here
            null;
         end case;
      when Not_An_Element =>
         --  Nothing to do here
         null;
      end case;

      Json_Element.Set_Field (Export_Utils.IS_ENCLOSED_IN'Image, Json_Parent);
      Json_Element.Set_Field ("kind", Get_Kind_Image (Element));
      Json_Element.Set_Field ("kinds", Kinds);
      Json_Element.Set_Field ("node_id", Get_Node_Id (Json_Element));
      Export_Manager.Notify (Json_Element);
   end Pre_Procedure;

   --------------------
   -- Post_Procedure --
   --------------------

   procedure Post_Procedure (Element : in Asis.Element) is
   begin
      --  Currently, there is nothing to do
      null;
   end Post_Procedure;

   ---------------------
   -- Text_Enter_Unit --
   ---------------------

   procedure Text_Enter_Unit (Unit : in Asis.Compilation_Unit) is
      pragma Unreferenced (Unit);
   begin
--       rules.header_comments. enter_unit;
--        Log.Info ("Text_Enter_Unit");
      null;
   end Text_Enter_Unit;

   -------------------
   -- Text_Analysis --
   -------------------

   procedure Text_Analysis (Line : Asis.Program_Text; Loc : Location) is
   begin
--       rules.characters.       process_line (line, loc);
--       rules.header_comments.  process_line (line, loc);
--       rules.max_blank_lines.  process_line (line, loc);
--       rules.max_line_length.  process_line (line, loc);
--       rules.comments.         process_line (line, loc);
--       rules.style.            process_line (line, loc);
--        Log.Info ("Text_Analysis");
      null;
   end Text_Analysis;
end Framework.Plugs;
