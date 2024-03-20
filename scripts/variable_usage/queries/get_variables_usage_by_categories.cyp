MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (vD:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
  WHERE (vD)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (v)-[:LOCATED_IN]->(l:Location)

WITH *,
  CASE WHEN size([(v)-[:IS_ENCLOSED_IN*]->(decl:A_GENERIC_PACKAGE_DECLARATION) | decl]) = 0 THEN v ELSE null END AS normalVar,
  CASE WHEN size([(v)-[:IS_ENCLOSED_IN*]->(decl:A_GENERIC_PACKAGE_DECLARATION) | decl]) > 0 THEN v ELSE null END AS genericVar

//////////////////////
// Normal variables //
//////////////////////

// Find read variables
CALL {
  WITH normalVar
  MATCH readVar=(normalVar)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
    WHERE NOT EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
    AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
    AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
  WITH collect(readVar) as readArray
  RETURN size(readArray) > 0 AS isReadNormal
}

// Find written variables
CALL {
  WITH normalVar
  MATCH writeVar=(normalVar)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
    WHERE (EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
    OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
    OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_IN_OUT_MODE)))
    AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
  WITH collect(writeVar) as writeArray
  RETURN size(writeArray) > 0 AS isWriteNormal
}

///////////////////////
// Generic variables //
///////////////////////

// Find read variables
CALL {
  WITH genericVar
  MATCH readVar=(genericVar)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
    WHERE NOT EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
    AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
    AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
    AND NOT EXISTS ((elt)-[:CORRESPONDING_INSTANCIATION]->())
  WITH collect(readVar) as readArray
  RETURN size(readArray) > 0 AS isReadGen
}

// Find written variables
CALL {
  WITH genericVar
  MATCH writeVar=(genericVar)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
    WHERE (EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
    OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
    OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_IN_OUT_MODE)))
    AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
    AND NOT EXISTS ((elt)-[:CORRESPONDING_INSTANCIATION]->())
  WITH collect(writeVar) as writeArray
  RETURN size(writeArray) > 0 AS isWriteGen
}

// Aggregate all results for the final result

CALL {
  // Normal
  WITH cu, l, normalVar, isWriteNormal, isReadNormal, genericVar, isWriteGen, isReadGen
  MATCH (normalVar)
  WHERE normalVar IS NOT NULL
  RETURN cu as Compilation_Unit, l as Location, normalVar as Variable, isWriteNormal AS isWrite, isReadNormal AS isRead,
         "normal" AS origin

  UNION

  // Generic
  WITH cu, l, genericVar, isWriteGen, isReadGen
  MATCH (genericVar)
    WHERE genericVar IS NOT NULL
  RETURN cu as Compilation_Unit, l as Location, genericVar as Variable, isWriteGen AS isWrite, isReadGen AS isRead,
         "generic" AS origin
}

RETURN DISTINCT Compilation_Unit, Location, Variable, isWrite, isRead, origin
  ORDER BY Location.filename, Location.line, Location.column;
