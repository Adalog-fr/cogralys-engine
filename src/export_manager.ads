with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with GNATCOLL.JSON;

package Export_Manager is
    use Ada.Strings.Unbounded, GNATCOLL.JSON;

    type Export_Event_Listener is task interface;
    subtype EEM_Class is Export_Event_Listener'Class;
    type EEM_Class_Ptr is access EEM_Class;

    procedure Update (This : Export_Event_Listener; Element : JSON_Value) is abstract;
    procedure Finalize (This : Export_Event_Listener) is abstract;

    type Observer_Type is tagged record
        Name : Unbounded_String;
        Job  : EEM_Class_Ptr;
    end record;

    function "=" (A, B : Observer_Type) return Boolean is (A.Name = B.Name);
    function Hash (Element : Observer_Type) return Ada.Containers.Hash_Type is
       (Ada.Strings.Unbounded.Hash (Element.Name));

    procedure Subscribe (Observer : Observer_Type);
    procedure Unsubscribe (Observer : Observer_Type);

    procedure Notify (Element : JSON_Value);
    procedure Finalize;

end Export_Manager;
