MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (vD:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
  WHERE (vD)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (l:Location)<-[:LOCATED_IN]-(v)

// Find read variables
CALL {
WITH v
MATCH readVar=(v)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
  WHERE NOT EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
WITH collect(readVar) as readArray
RETURN size(readArray) > 0 AS isRead1
}

// Find written variables
CALL {
WITH v
MATCH writeVar=(v)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
  WHERE (EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
  OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
  OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_IN_OUT_MODE)))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
WITH collect(writeVar) as writeArray
RETURN size(writeArray) > 0 AS isWrite1
}

// Return the result of this amazing query: tell me if a variable is read or write !
RETURN cu as Compilation_Unit, l as Location, v as Variable, isWrite1 AS isWrite, FALSE OR isRead1 AS isRead
  ORDER BY l.filename, l.line, l.column

//////////////////
// Get generics //
//////////////////

UNION
MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (vD:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
  WHERE (vD)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (l:Location)<-[:LOCATED_IN]-(v)
// Find read variables
CALL {
WITH v
MATCH p=(v)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
  WHERE NOT EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_IN_OUT_MODE))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
with collect(p) as nb
return size(nb) > 0 AS isRead1
}
// Find written variables
CALL {
return FALSE AS isWrite1
}
CALL {
WITH v
MATCH (v)<-[:CORRESPONDING_NAME_DEFINITION]-(i:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT)
return EXISTS((i)-[:CORRESPONDING_INSTANCIATION]->()-[:LOCATED_IN]->(:Location)) AS isWrite2
}
// Return the result of this amazing query: tell me if a variable is read or write !
RETURN cu as Compilation_Unit, l as Location, v as Variable, isWrite1 or isWrite2 AS isWrite, FALSE as isRead

/////////////////////////////
// Get instancied generics //
/////////////////////////////

UNION
MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (vD:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
  WHERE (vD)-[:IS_ENCLOSED_IN*]->(cu)
MATCH(v)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:CORRESPONDING_INSTANCIATION]->()-[:LOCATED_IN]->(l:Location)
// Find read variables
CALL {
WITH v
MATCH p=(v)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
  WHERE NOT EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_IN_OUT_MODE))
  AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
with collect(p) as nb
return size(nb) > 0 AS isRead1
}
// Find written variables
CALL {
return FALSE AS isWrite1
}
CALL {
WITH v
return EXISTS((v)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT)) AS isWrite2
}
// Return the result of this amazing query: tell me if a variable is read or write !
RETURN cu as Compilation_Unit, l as Location, v as Variable, isWrite1 or isWrite2 AS isWrite, FALSE as isRead
