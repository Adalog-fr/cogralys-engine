MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (vD:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
  WHERE (vD)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (v)-[:LOCATED_IN]->(l:Location)

// Match any variable not declared inside a generic package
OPTIONAL MATCH normalVar = (vD)-[:IS_ENCLOSED_IN*]->(:A_PACKAGE_DECLARATION)

// Match any variable declared inside a generic package (opposite of last query)
OPTIONAL MATCH genVar = (vD)-[:IS_ENCLOSED_IN*]->(genPackDecl:A_GENERIC_PACKAGE_DECLARATION)

// For normal variable, check if it is read and write

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

RETURN cu as Compilation_Unit, l as Location, v as Variable
  ORDER BY l.filename, l.line, l.column
