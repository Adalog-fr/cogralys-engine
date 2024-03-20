// Find any instantiation of generics and get their corresponding generics

// This query is not efficient because it scan all nodes before applying the label filter
MATCH (inst)<-[:IS_ENCLOSED_IN { index: 2 }]-(gen)-[:CORRESPONDING_NAME_DEFINITION]->(genPack)
WHERE any (x IN labels(inst) WHERE x IN
['A_FORMAL_PACKAGE_DECLARATION','A_FORMAL_PACKAGE_DECLARATION_WITH_BOX','A_PACKAGE_INSTANTIATION'])
RETURN *;

// This is the most efficient approach
/*
 * This will perform a label scan for the labels A_FORMAL_PACKAGE_DECLARATION, A_FORMAL_PACKAGE_DECLARATION_WITH_BOX,
 * A_PACKAGE_INSTANTIATION, instead of all node scan which happens with the previous solution.
 */
MATCH (inst)<-[:IS_ENCLOSED_IN { index: 2 }]-(gen)-[:CORRESPONDING_NAME_DEFINITION]->(genPackId)
WHERE (inst:A_FORMAL_PACKAGE_DECLARATION) OR (inst:A_FORMAL_PACKAGE_DECLARATION_WITH_BOX) OR (inst:A_PACKAGE_INSTANTIATION)
MATCH (genPackId)-[:IS_ENCLOSED_IN*]->(genPackDecl:A_GENERIC_PACKAGE_DECLARATION)
RETURN *;

/////////////

//////////////////////////////
// Match written parameters //
//////////////////////////////

MATCH (p:AN_IDENTIFIER)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(paramSpec)
  WHERE (paramSpec:AN_OUT_MODE) OR (paramSpec:AN_IN_OUT_MODE)
RETURN *;

//////////////////////////
// Match read variables //
//////////////////////////

MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (vD:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
  WHERE (vD)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (l:Location)<-[:LOCATED_IN]-(v)
MATCH (v)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
  WHERE NOT EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_IN_OUT_MODE))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
RETURN *;

/////////////////////////////
// Match written variables //
/////////////////////////////

// Match any A_DEFINING_IDENTIFIER of A_VARIABLE_DECLARATION inside A_GENERIC_PACKAGE_DECLARATION, who have an instantiation
MATCH (inst)<-[:IS_ENCLOSED_IN { index: 2 }]-(gen)-[:CORRESPONDING_NAME_DEFINITION]->(genPackId)-[:IS_ENCLOSED_IN]->(:Public_Part)-[:IS_ENCLOSED_IN]->(genPack:A_GENERIC_PACKAGE_DECLARATION)
  WHERE (inst:A_FORMAL_PACKAGE_DECLARATION) OR (inst:A_FORMAL_PACKAGE_DECLARATION_WITH_BOX) OR (inst:A_PACKAGE_INSTANTIATION)
MATCH (genPack)<-[:IS_ENCLOSED_IN*]-(vD:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
MATCH (loc:Location)<-[:LOCATED_IN]-(inst)
RETURN v, loc;

// Extend the previous request to tell if variables are written
MATCH (inst)<-[:IS_ENCLOSED_IN { index: 2 }]-(gen)-[:CORRESPONDING_NAME_DEFINITION]->(genPackId)-[:IS_ENCLOSED_IN]->(:Public_Part)-[:IS_ENCLOSED_IN]->(genPack:A_GENERIC_PACKAGE_DECLARATION)
  WHERE (inst:A_FORMAL_PACKAGE_DECLARATION) OR (inst:A_FORMAL_PACKAGE_DECLARATION_WITH_BOX) OR (inst:A_PACKAGE_INSTANTIATION)
MATCH (genPack)<-[:IS_ENCLOSED_IN*]-(vD:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
MATCH (loc:Location)<-[:LOCATED_IN]-(inst)

// Get write in sub-program parameters
//CALL {
//WITH v, inst
//MATCH (p:AN_IDENTIFIER)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(paramSpec)
//  WHERE (paramSpec:AN_OUT_MODE) OR (paramSpec:AN_IN_OUT_MODE)
//RETURN EXISTS((p)-[:CORRESPONDING_INSTANCIATION]->()) AS isWriteSubProgParam
//}

// Get write in generic
CALL {
WITH v, inst
MATCH (v)<-[:CORRESPONDING_NAME_DEFINITION]-(i:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT)
RETURN NOT EXISTS((i)-[:CORRESPONDING_INSTANCIATION]->()) AS isWrite1
}

// Get write in instantiation
CALL {
WITH v, inst
MATCH (v)<-[:CORRESPONDING_NAME_DEFINITION]-(i:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT)
RETURN EXISTS((i)-[:CORRESPONDING_INSTANCIATION]->(inst)-[:LOCATED_IN]->(:Location)) AS isWrite2
}

RETURN v, loc, isWriteSubProgParam OR isWrite2 OR isWrite2 AS isWrite;
