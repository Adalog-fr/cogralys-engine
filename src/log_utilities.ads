with Asis; use Asis;
with Util.Log; use Util.Log;
with Util.Log.Loggers; use Util.Log.Loggers;

package Log_Utilities is
    procedure Print
       (Log : Logger'Class; Level : Level_Type; Message : String;
       Element : Asis.Element; With_Source : Boolean := True);

    procedure Debug (Log : Logger'Class; Message : String; Element : Asis.Element; With_Source : Boolean := True);

    procedure Info (Log : Logger'Class; Message : String; Element : Asis.Element; With_Source : Boolean := True);

    procedure Warn (Log : Logger'Class; Message : String; Element : Asis.Element; With_Source : Boolean := True);

    procedure Error (Log : Logger'Class; Message : String; Element : Asis.Element; With_Source : Boolean := True);
end Log_Utilities;
