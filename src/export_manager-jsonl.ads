-- This package export the AST in JSON (internal JSON representation).
-- It uses the JSONLines format, and generate a `database.jsonl` in the current working directory.
-- {@link https://jsonlines.org}

package Export_Manager.JSONL is

    task type Instance is new Export_Event_Listener with
        entry Update (Element : JSON_Value);
        entry Finalize;
    end Instance;

    function Initialize return Observer_Type;

end Export_Manager.JSONL;
