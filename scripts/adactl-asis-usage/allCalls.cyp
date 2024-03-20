MATCH (caller)<-[:CORRESPONDING_SPECIFICATION]-(callerBody:A_FUNCTION_BODY_DECLARATION|A_FUNCTION_BODY_STUB|A_PROCEDURE_BODY_DECLARATION|A_PROCEDURE_BODY_STUB|A_PROTECTED_BODY_DECLARATION|A_PROTECTED_BODY_STUB|A_TASK_BODY_DECLARATION|A_TASK_BODY_STUB|AN_ENTRY_BODY_DECLARATION|AN_EXPRESSION_FUNCTION_DECLARATION)
//WHERE caller.enclosing_unit contains "Max_Statement_Nesting"
WITH caller
ORDER BY caller.filename, caller.line, caller.column

MATCH (callerId)-[:IS_ENCLOSED_IN { index: 1 }]->(caller)-[r:CALLING]->(n)<-[:IS_ENCLOSED_IN { index: 1 }]-(nId)

RETURN caller, callerId, collect ({node:n, nodeId: nId, nbCall: r.nbCall}) as called
ORDER BY caller.filename, caller.line, caller.column
