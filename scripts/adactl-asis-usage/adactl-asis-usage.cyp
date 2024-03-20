MATCH (caller)<-[:CORRESPONDING_SPECIFICATION]-(callerBody:A_FUNCTION_BODY_DECLARATION|A_FUNCTION_BODY_STUB|A_PROCEDURE_BODY_DECLARATION|A_PROCEDURE_BODY_STUB|A_PROTECTED_BODY_DECLARATION|A_PROTECTED_BODY_STUB|A_TASK_BODY_DECLARATION|A_TASK_BODY_STUB|AN_ENTRY_BODY_DECLARATION|AN_EXPRESSION_FUNCTION_DECLARATION)
WHERE callerBody.enclosing_unit STARTS WITH "Rules."
    AND callerBody.filename ENDS WITH ".adb"
WITH caller
ORDER BY caller.filename, caller.line, caller.column
// LIMIT 10

CALL {
    WITH caller
    MATCH (caller)-[r:CALLING*]->(n)
    // MATCH (caller)-[r:CALLING]->(n)
    WITH DISTINCT n
    // WITH n,
    // apoc.coll.sum([(n)<-[rel:CALLING]-() | rel.nbCall]) AS nbCall
    MATCH (nId)-[:IS_ENCLOSED_IN { index: 1 }]->(n)
    WITH DISTINCT nId
    // RETURN nId as node, nbCall
    RETURN nId as node
    ORDER BY nId.filename, nId.line, nId.column
}
// RETURN caller, collect ({node: node, nbCall: nbCall}) as called
RETURN caller, collect ({node: node}) as called
ORDER BY caller.filename, caller.line, caller.column
