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

  UNION

  // Instance
  WITH cu, genericVar
  CALL {
    WITH cu, genericVar
    // WITH cu, [x in collect(genericVar) WHERE x is not null | x] as genericVars
    UNWIND [val in genericVar WHERE val IS NOT NULL] as genVar

    // match instantiated generic packages

    CALL {
      // WITH genericVars
      WITH genVar
      WITH genVar.var as var, genVar.decl as decl
      // // First, get variables declared inside the generic package
      // MATCH (var:A_DEFINING_IDENTIFIER)-[:IS_ENCLOSED_IN]->(vD:A_VARIABLE_DECLARATION)-[:IS_ENCLOSED_IN*]->(decl:A_GENERIC_PACKAGE_DECLARATION)
      //     WHERE var in genericVars

      ///////////////////////
      // Generic variables //
      ///////////////////////

      // Find read variables
      CALL {
        WITH var
        MATCH readVar=(var)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
        WHERE NOT EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
        AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
        AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
        AND NOT EXISTS ((elt)-[:CORRESPONDING_INSTANCIATION]->())
        WITH collect(readVar) as readArray
        RETURN size(readArray) > 0 AS isReadGen
      }

      // Find written variables
      CALL {
        WITH var
        MATCH writeVar=(var)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
        WHERE (EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
        OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
        OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_IN_OUT_MODE)))
        AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
        AND NOT EXISTS ((elt)-[:CORRESPONDING_INSTANCIATION]->())
        WITH collect(writeVar) as writeArray
        RETURN size(writeArray) > 0 AS isWriteGen
      }

      WITH decl, collect({ var: var, isWriteGen: isWriteGen, isReadGen: isReadGen }) as vars

      // Then, find every instantiation of this generic package
      MATCH (inst)<-[:IS_ENCLOSED_IN { index: 2 }]-(gen)-[:CORRESPONDING_NAME_DEFINITION]->(genPackId)
          WHERE (inst:A_FORMAL_PACKAGE_DECLARATION) OR (inst:A_FORMAL_PACKAGE_DECLARATION_WITH_BOX) OR (inst:A_PACKAGE_INSTANTIATION)

      WITH decl, vars, collect(CASE WHEN exists((genPackId)-[:IS_ENCLOSED_IN*]->(decl)) THEN inst ELSE null END) AS instRef

      RETURN { decl: decl, vars: vars, instances: instRef } AS genPackMap
      // return null
    } // END: match instantiated generic packages

    // Find usage of every instantiations

    CALL {
        with genPackMap
        WITH genPackMap.instances as instances, genPackMap.vars as vars
        // UNWIND genPackMap as genPack
        UNWIND instances AS instance
        // UNWIND [instance in genPackMap.instances WHERE instance IS NOT NULL] as genVar

        // Build result of var in instance
        CALL {
            WITH vars, instance
            UNWIND vars AS var

            // Find read variables
            CALL {
            WITH var, instance
            WITH var.var as var, instance
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
            WITH var.var as var, instance
            MATCH writeVar=(var)<-[:CORRESPONDING_NAME_DEFINITION]-(elt:AN_IDENTIFIER)
                WHERE (EXISTS((elt:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
                OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_OUT_MODE))
                OR EXISTS ((elt)-[:IS_ENCLOSED_IN]->(:A_PARAMETER_ASSOCIATION)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(:AN_IN_OUT_MODE)))
                AND NOT EXISTS ((elt)-[:IS_ENCLOSED_IN*]->(:AN_OBJECT_RENAMING_DECLARATION))
                AND EXISTS ((elt)-[:CORRESPONDING_INSTANCIATION]->(instance))
            WITH collect(writeVar) as writeArray
            RETURN size(writeArray) > 0 AS isWriteInst
            }

            RETURN  var.var as Variable, isWriteInst, isReadInst, var.isWriteGen AS isWriteGen, var.isReadGen AS isReadGen
        } // END: Build result of var in instance

        return { filename: instance.filename, line: instance.line, column: instance.column } as Location, Variable, isWriteInst, isReadInst, isWriteGen, isReadGen
    } // END: Find usage of every instantiations

    RETURN cu as Compilation_Unit,
        null as Location,
        null as Variable,
        False AS isWrite,
        False AS isRead,
        // isWriteInst OR isWriteGen AS isWrite,
        // isReadInst OR isReadGen AS isRead,
        "instance" AS origin
  } // END: Union Instance

  RETURN Compilation_Unit,
        Location,
        Variable,
        isWrite,
        isRead,
        origin
} // END: Aggregate all results for the final result

RETURN DISTINCT Compilation_Unit, Location, Variable, isWrite, isRead, origin
  ORDER BY Location.filename, Location.line, Location.column;
