MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (vD:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
  WHERE (vD)-[:IS_ENCLOSED_IN*]->(cu)

OPTIONAL MATCH enclGen = (v)-[:IS_ENCLOSED_IN*]->(decl:A_GENERIC_PACKAGE_DECLARATION)

WITH *,
  CASE WHEN length(enclGen) is null THEN v ELSE null END AS normalVar,
  CASE WHEN length(enclGen) is not null THEN { var: v, decl: decl } ELSE NULL END AS genericVar,
  { filename: v.filename, line: v.line, column: v.column } as l

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
  WITH genericVar.var as genVar
  MATCH readVar=(genVar)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
    WHERE NOT EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
    AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
    AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
  WITH collect(readVar) as readArray
  RETURN size(readArray) > 0 AS isReadGen
}

// Find written variables
CALL {
  WITH genericVar
  WITH genericVar.var as genVar
  MATCH writeVar=(genVar)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
    WHERE (EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
    OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
    OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_IN_OUT_MODE)))
    AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
  WITH collect(writeVar) as writeArray
  RETURN size(writeArray) > 0 AS isWriteGen
}

////////////
// Result //
////////////

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
  UNWIND [val in genericVar WHERE val IS NOT NULL] as genVar
  WITH cu, l, isWriteGen, isReadGen, genVar.var AS finalGenericVar
  MATCH (finalGenericVar)

  RETURN cu as Compilation_Unit, l as Location, finalGenericVar as Variable, isWriteGen AS isWrite, isReadGen AS isRead,
          "generic" AS origin
} // END: Aggregate all results for the final result

RETURN DISTINCT Compilation_Unit, Location, Variable, isWrite, isRead, origin
  ORDER BY Location.filename, Location.line, Location.column;
