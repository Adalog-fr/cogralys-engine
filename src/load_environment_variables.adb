with Ada.Environment_Variables;
with Dotenv;
with Util.Log.Loggers;

package body Load_Environment_Variables is
begin
   Dotenv.Config_Interpolate (True);

   if Ada.Environment_Variables.Exists ("LOGGER_CONFIG") then
      Util.Log.Loggers.Initialize (Ada.Environment_Variables.Value ("LOGGER_CONFIG"));
   end if;
end Load_Environment_Variables;
