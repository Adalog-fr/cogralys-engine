package Export_Manager.Neo4J is

    task type Instance is new Export_Event_Listener with
        entry Update (Element : JSON_Value);
        entry Finalize;
    end Instance;

    function Initialize return Observer_Type;

end Export_Manager.Neo4J;
