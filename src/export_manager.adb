with Ada.Containers.Hashed_Sets;

package body Export_Manager is

    package EEM_Vectors is new Ada.Containers.Hashed_Sets
       (Element_Type => Observer_Type, Hash => Hash, Equivalent_Elements => "=", "=" => "=");

    Observers : EEM_Vectors.Set := EEM_Vectors.Empty_Set;

    procedure Subscribe (Observer : Observer_Type) is
    begin
        EEM_Vectors.Include (Observers, Observer);
    end Subscribe;

    procedure Unsubscribe (Observer : Observer_Type) is
    begin
        Observers.Delete (Observer);
    end Unsubscribe;

    procedure Notify (Element : JSON_Value) is
    begin
        for Observer : Observer_Type of Observers loop
            Observer.Job.Update (Element);
        end loop;
    end Notify;

    procedure Finalize is
    begin
        for Observer : Observer_Type of Observers loop
            Observer.Job.Finalize;
        end loop;
    end Finalize;
end Export_Manager;
