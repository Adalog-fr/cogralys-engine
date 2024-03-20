with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Observer_Example is

    type Observer is interface;
    type Subject is interface;

    type Observer_Access is access all Observer;
    type Subject_Access is access all Subject;

    type Observer is interface;
    procedure Update (Subject : in Subject_Access);

    type Subject is interface;
    procedure Attach (Observer : in out Observer_Access);
    procedure Detach (Observer : in out Observer_Access);
    procedure Notify;

    type Concrete_Observer is new Observer with record
        Name : String (1 .. 10);
    end record;

    procedure Update (Subject : in Subject_Access) is
    begin
        Ada.Text_IO.Put_Line (Concrete_Observer'Access.Name & " a été mis à jour!");
    end Update;

    type Concrete_Subject is new Subject with record
        Observers : Ada.Containers.Vectors.Vector (Observer_Access);
    end record;

    procedure Attach (Observer : in out Observer_Access) is
    begin
        Concrete_Subject'Access.Observers.Append (Observer);
    end Attach;

    procedure Detach (Observer : in out Observer_Access) is
    begin
        Concrete_Subject'Access.Observers.Delete (Observer);
    end Detach;

    procedure Notify is
    begin
        for Observer in Concrete_Subject'Access.Observers.Iterate loop
            Observer.Update (Concrete_Subject'Access);
        end loop;
    end Notify;

begin
    -- Créer un sujet
    declare
        Subject              : Concrete_Subject;
        Observer1, Observer2 : Concrete_Observer;
    begin
        Observer1.Name := "Observateur 1";
        Observer2.Name := "Observateur 2";

        Subject.Attach (Observer1'Access);
        Subject.Attach (Observer2'Access);

        Subject.Notify;

        Subject.Detach (Observer1'Access);
        Subject.Notify;
    end;
end Observer_Example;
