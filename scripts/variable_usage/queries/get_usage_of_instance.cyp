MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (vD:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
  WHERE (vD)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (v)-[:LOCATED_IN]->(l:Location)

WITH *,
  CASE WHEN size([(v)-[:IS_ENCLOSED_IN*]->(decl:A_GENERIC_PACKAGE_DECLARATION) | decl]) = 0 THEN v ELSE null END AS normalVar,
  CASE WHEN size([(v)-[:IS_ENCLOSED_IN*]->(decl:A_GENERIC_PACKAGE_DECLARATION) | decl]) > 0 THEN v ELSE null END AS genericVar
WITH cu, [x in collect(genericVar) WHERE x is not null | x] as genericVars

// match instantiated generic packages

CALL {
WITH genericVars
// First, get variables declared inside the generic package
MATCH (var:A_DEFINING_IDENTIFIER)-[:IS_ENCLOSED_IN]->(vD:A_VARIABLE_DECLARATION)-[:IS_ENCLOSED_IN*]->(decl:A_GENERIC_PACKAGE_DECLARATION)
  WHERE var in genericVars
WITH decl, collect(var) as vars

// Then, find every instantiation of this generic package
MATCH (inst)<-[:IS_ENCLOSED_IN { index: 2 }]-(gen)-[:CORRESPONDING_NAME_DEFINITION]->(genPackId)
  WHERE (inst:A_FORMAL_PACKAGE_DECLARATION) OR (inst:A_FORMAL_PACKAGE_DECLARATION_WITH_BOX) OR (inst:A_PACKAGE_INSTANTIATION)

WITH decl, vars, CASE WHEN exists((genPackId)-[:IS_ENCLOSED_IN*]->(decl)) THEN inst ELSE null END AS instRef

RETURN { decl: decl, vars: vars, instances: collect(instRef) } AS genPackMap
}

//RETURN genPackMap

// Find usage of every instantiations

CALL {
  WITH genPackMap
  UNWIND genPackMap.instances AS instance
  MATCH (instance)-[:LOCATED_IN]->(instLoc:Location)

  CALL {
    WITH genPackMap, instance, instLoc
    UNWIND genPackMap.vars AS var

    // Find read variables
    CALL {
      WITH var, instance
      MATCH readVar=(var)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
        WHERE NOT EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
        AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
        AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
        AND EXISTS ((elt)-[:CORRESPONDING_INSTANCIATION]->(instance))
      WITH collect(readVar) as readArray
      RETURN size(readArray) > 0 AS isReadInst
    }

    // Find written variables
    CALL {
      WITH var, instance
      MATCH writeVar=(var)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
        WHERE (EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
        OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
        OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_IN_OUT_MODE)))
        AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
        AND EXISTS ((elt)-[:CORRESPONDING_INSTANCIATION]->(instance))
      WITH collect(writeVar) as writeArray
      RETURN size(writeArray) > 0 AS isWriteInst
    }

    RETURN  var as Variable, isWriteInst, isReadInst
  }

  return instLoc as Location, Variable, isWriteInst, isReadInst
}

RETURN cu as Compilation_Unit, Location, Variable, isWriteInst, isReadInst, "instance" AS origin
