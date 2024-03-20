----------------------------------------------------------------------
--  Framework.Plugs - Package specification                         --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
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

with Ada.Containers.Vectors;
with Framework.Locations;
with Asis;
with Asis.Elements;
with GNATCOLL.JSON;

package Framework.Plugs is
   use Framework.Locations;

   type Parameters_Info is private;

   type Visibility_T is (V_Normal, V_Public, V_Private);

   -- Info type for Semantic_Traverse:
   type Info is record
      Pragma_Or_Attribute_Level          : Natural;
      Visibility                         : Visibility_T;
      Corresponding_Assignment_Statement : Asis.Element;
      Parameters_Information             : Parameters_Info;
   end record;

   -- Pragma_Or_Attribute_Level:
   -- Used to trace whether we are in a pragma or attribute (see procedure True_Identifer);
   -- We need a counter rather than a boolean, because attributes may have multiple levels
   -- (i.e. T'Base'First)
   --
   -- Child_Index:
   -- Used to know the orders of children of an element.

   procedure Enter_Unit           (Unit    : in Asis.Compilation_Unit);
   procedure Exit_Context_Clauses (Unit    : in Asis.Compilation_Unit);
   procedure Exit_Unit            (Unit    : in Asis.Compilation_Unit);
   procedure Enter_Scope          (Element : in Asis.Element);
   procedure Enter_Public_Part    (Element : in Asis.Element);
   procedure Enter_Private_Part   (Element : in Asis.Element);
   procedure Exit_Scope           (Element : in Asis.Element);
   procedure Enter_Statement_List (Element : in Asis.Element);
   procedure Enter_Subprogram_Call (Element : in Asis.Element; Normalized : Boolean := False);

   procedure True_Identifier (Element                     : in Asis.Expression;
                              Corresponding_Instanciation : out Asis.Declaration);

   procedure Pre_Procedure  (Element                     : in Asis.Element;
                             State                       : in out Info;
                             Corresponding_Instantiation : in Asis.Element := Asis.Nil_Element);
   procedure Post_Procedure (Element : in     Asis.Element);

   procedure Text_Enter_Unit (Unit : in Asis.Compilation_Unit);
   procedure Text_Analysis   (Line : Asis.Program_Text; Loc : Location);

   function Create_Nil_Parameters_Info return Parameters_Info;
   function Create_Parameters_Info (Call : Asis.Element; Normalized : Boolean := False) return Parameters_Info;

private
   type Parameter_Info is tagged record
      Formal_Name                    : Asis.Association;
      Index                          : Positive;
      Is_Named_Parameter_Association : Boolean;
   end record;

   function "=" (Left, Right : Parameter_Info) return Boolean is
     (Asis.Elements.Is_Equal (Left.Formal_Name, Right.Formal_Name) and Left.Index = Right.Index);

   package Association_Vector is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                             Element_Type => Parameter_Info,
                                                             "="          => "=");
   use Association_Vector;

   type Parameter_Association is record
      Association                     : GNATCOLL.JSON.JSON_Value;
      Corresponding_Actual_Expression : GNATCOLL.JSON.JSON_Value;
   end record;

   Nil_Parameter_Association : constant Parameter_Association := (GNATCOLL.JSON.JSON_Null, GNATCOLL.JSON.JSON_Null);

   type Parameters_Info is tagged record
      Association             : Parameter_Association;
      Elements                : Vector;
      Current_Primitive_Index : Natural; -- 0 = named notation; When > 0, positionnal notation
      Normalized              : Boolean;
   end record;

   Nil_Parameters_Info : constant Parameters_Info := (Nil_Parameter_Association,
                                                      Empty_Vector,
                                                      Current_Primitive_Index => 1,
                                                      Normalized              => False);

end Framework.Plugs;
