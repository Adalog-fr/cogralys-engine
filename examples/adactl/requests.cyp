// Match all
MATCH (n) RETURN n;

// Delete all
MATCH (n) DETACH DELETE n;

// Match Defining Names
MATCH (n)
  WHERE n.element_kind = 'A_DEFINING_NAME'
RETURN n;

// Match element that should have a Defining Name
MATCH (n:Element)
  WHERE n.element_kind = 'AN_EXPRESSION' AND n.kind in ['AN_IDENTIFIER', 'AN_OPERATOR_SYMBOL', 'A_CHARACTER_LITERAL', 'AN_ENUMERATION_LITERAL']
  AND n.special_case <> 'FROM_LIMITED_VIEW'
RETURN n;

// Match All element that should have a Defining Name and
// return this node with the Corresponding Name Definition
MATCH (def)
  WHERE def.element_kind = 'A_DEFINING_NAME'
MATCH (n:Element)-[r:CORRESPONDING_NAME_DEFINITION]->(def)
  WHERE n.element_kind = 'AN_EXPRESSION' AND n.kind in ['AN_IDENTIFIER', 'AN_OPERATOR_SYMBOL', 'A_CHARACTER_LITERAL', 'AN_ENUMERATION_LITERAL']
  AND n.special_case <> 'FROM_LIMITED_VIEW'
RETURN n, r, def;

/*
 * Match every types that have 4 and more primitives operations.
 * Note that this query is not fully complete. It only match "First Degree" types.
 * It doesn't follow ancestors to check them (interface, etc.).
 */
MATCH (n:A_PACKAGE_DECLARATION)<-[:IS_ENCLOSED_IN]-(elt:Element)-[:LOCATED_IN]->(eltLoc:Location)
  WHERE elt.kind in ['A_PROCEDURE_DECLARATION', 'A_FUNCTION_DECLARATION', 'A_NULL_PROCEDURE_DECLARATION', 'AN_EXPRESSION_FUNCTION_DECLARATION']
MATCH (elt)<-[:IS_ENCLOSED_IN]-(:A_PARAMETER_SPECIFICATION)<-[:IS_ENCLOSED_IN]-(:AN_IDENTIFIER)-[:CORRESPONDING_NAME_DEFINITION]->(def:A_DEFINING_IDENTIFIER)-[:LOCATED_IN]->(defLoc:Location)
  WHERE eltLoc.filename = defLoc.filename
WITH def, collect(distinct elt) as primitives, count(distinct elt) as nbPrimitives, defLoc, eltLoc
  WHERE nbPrimitives >= 4
RETURN def, primitives, nbPrimitives
ORDER BY defLoc.filename,  defLoc.line,  defLoc.column, eltLoc.filename, eltLoc.line, eltLoc.column;

// Sort values

MATCH (n:A_PACKAGE_DECLARATION)<-[:IS_ENCLOSED_IN]-(elt:Element)-[:LOCATED_IN]->(eltLoc:Location)
  WHERE elt.kind in ['A_PROCEDURE_DECLARATION', 'A_FUNCTION_DECLARATION', 'A_NULL_PROCEDURE_DECLARATION', 'AN_EXPRESSION_FUNCTION_DECLARATION']
MATCH (elt)<-[:IS_ENCLOSED_IN]-(:A_PARAMETER_SPECIFICATION)<-[:IS_ENCLOSED_IN]-(:AN_IDENTIFIER)-[:CORRESPONDING_NAME_DEFINITION]->(def:A_DEFINING_IDENTIFIER)-[:LOCATED_IN]->(defLoc:Location)
  WHERE eltLoc.filename = defLoc.filename
WITH def, defLoc, eltLoc, elt
  ORDER BY defLoc.filename, defLoc.line,  defLoc.column, eltLoc.filename, eltLoc.line, eltLoc.column
WITH def, collect(distinct elt) as primitives, count(distinct elt) as nbPrimitives
  WHERE nbPrimitives >= 4
RETURN def, primitives, nbPrimitives

// Match every types, and there ancestors (derived and interface)

MATCH (eltType:Element)
  WHERE eltType.kind in ['AN_ORDINARY_TYPE_DECLARATION', 'A_TASK_TYPE_DECLARATION', 'A_PROTECTED_TYPE_DECLARATION', 'AN_INCOMPLETE_TYPE_DECLARATION', 'A_TAGGED_INCOMPLETE_TYPE_DECLARATION', 'A_PRIVATE_TYPE_DECLARATION', 'A_PRIVATE_EXTENSION_DECLARATION']
MATCH (eltType)<-[:IS_ENCLOSED_IN]-(der:A_DERIVED_RECORD_EXTENSION_DEFINITION)<-[:IS_ENCLOSED_IN]-(:A_SUBTYPE_INDICATION)<-[IS_ENCLOSED_IN]-(extType:AN_IDENTIFIER)
MATCH (der)<-[:IS_ENCLOSED_IN]-(interfaceType:AN_IDENTIFIER)
MATCH (extType)-[r1:CORRESPONDING_NAME_DEFINITION]->(def1)
MATCH (interfaceType)-[r2:CORRESPONDING_NAME_DEFINITION]->(def2)
RETURN eltType, extType, def1, r1, collect(interfaceType) AS interfaces, collect(r2) AS CORRESPONDING_INTERFACE_NAME_DEFINITION, collect(def2) AS interfacesNameDefinition;

// Query used to check the creation of order of parameters
MATCH (elt)-[:IS_ENCLOSED_IN]->(asso:AN_ASSOCIATION)-[:IS_ENCLOSED_IN]->(uParam:User_Parameters)-[:IS_ENCLOSED_IN]->(pCall:A_PROCEDURE_CALL_STATEMENT)-[:LOCATED_IN]->(l:Location { filename: "/Volumes/Data/programmation_pro/These/asis-to-graphdb/Asis_To_GraphDB/examples/adactl/wks/t_usage.adb", line: 113, column: 4 })
MATCH (pCall)<-[:IS_ENCLOSED_IN]-(id:AN_IDENTIFIER)-[:CORRESPONDING_NAME_DEFINITION]->(defId:A_DEFINING_IDENTIFIER)-[:IS_ENCLOSED_IN]->(pDecl:A_DECLARATION)<-[:IS_ENCLOSED_IN]-(paramSpec:A_PARAMETER_SPECIFICATION)<-[:IS_ENCLOSED_IN]-(paramSpecElt:Element)
RETURN *

// Match usage of variables
// use AN_ASSIGNMENT_STATEMENT for write usage

MATCH (correspondingNameDefinitionType:A_DEFINING_IDENTIFIER { content: 'YY' })-[:LOCATED_IN]->(locCorrespondingNameDefinition:Location)
RETURN *;

// Match read variables (NOT COMPLETE!)
MATCH (e:Element)
  WHERE e.element_kind = 'AN_EXPRESSION'
RETURN e

MATCH (e:AN_INTEGER_LITERAL)
return *
UNION
MATCH (e:A_REAL_LITERAL)
return *;

// Match part of readable expressions
MATCH (e:Element)
  WHERE e.kind in ['AN_INTEGER_LITERAL', 'A_REAL_LITERAL', 'A_STRING_LITERAL']
RETURN e;

// Match case where where we have
MATCH (e:Element)-[:IS_ENCLOSED_IN]->(p1)
  WHERE e.kind in ['AN_INTEGER_LITERAL', 'A_REAL_LITERAL', 'A_STRING_LITERAL'] AND NOT p1.kind in ['A_CONSTANT_DECLARATION', 'A_DELAY_RELATIVE_STATEMENT', 'A_DISCRETE_RANGE', 'A_DISCRETE_SUBTYPE_DEFINITION', 'A_REAL_NUMBER_DECLARATION', 'A_RETURN_STATEMENT', 'A_SIMPLE_EXPRESSION_RANGE', 'A_VARIABLE_DECLARATION', 'AN_INTEGER_NUMBER_DECLARATION']
WITH e, collect (distinct CASE p1.kind
  WHEN 'AN_ASSIGNMENT_STATEMENT' then
[(p1)<-[:IS_ENCLOSED_IN]-(p2)-[:CORRESPONDING_NAME_DEFINITION]->(def) | def]
  WHEN 'AN_ASSOCIATION' then 2
  ELSE -1 // Unknown case
  END) as res
RETURN res, e, count(e) as nbReads;

// Reverse version of the previous query, to start from the variable def, and collect all READ usage
// CASE: AN_ASSIGNMENT_STATEMENT
// TODO: check if we get all results
MATCH (def)<-[:CORRESPONDING_NAME_DEFINITION]-()-[:IS_ENCLOSED_IN]->(p1)<-[:IS_ENCLOSED_IN]-(e)
WHERE e.kind in ['AN_INTEGER_LITERAL', 'A_REAL_LITERAL', 'A_STRING_LITERAL'] AND p1.kind = 'AN_ASSIGNMENT_STATEMENT'
RETURN DISTINCT def, collect(e) as READ_OP;

// Reverse version of the previous query, to start from the variable def, and collect all READ usage
// CASE: AN_ASSOCIATION
// TODO: check if we get all results
MATCH (lDef:Location)<-[:LOCATED_IN]-(def)<-[:CORRESPONDING_NAME_DEFINITION]-()-[:IS_ENCLOSED_IN]->()<-[:IS_ENCLOSED_IN]-(p1)<-[:IS_ENCLOSED_IN]-(e)-[:LOCATED_IN]->(l:Location)
WHERE e.kind in ['AN_INTEGER_LITERAL', 'A_REAL_LITERAL', 'A_STRING_LITERAL'] AND p1.kind = 'AN_ASSOCIATION'
RETURN DISTINCT def, collect((e)-[:LOCATED_IN]->(l)) as READ_OP, lDef.line as Def_Location_Line, lDef.column as Def_Location_Column, lDef.filename as Def_Location_Filename
ORDER BY Def_Location_Filename,  Def_Location_Line,  Def_Location_Column;


// Match every WRITE usage of variables
MATCH (v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(cndV:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:IS_ENCLOSED_IN]->(u:AN_ASSIGNMENT_STATEMENT)
RETURN DISTINCT v, collect(u) as WRITE_VARIABLE;

MATCH (l:Location)<-[:LOCATED_IN]-(v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(cndV:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:IS_ENCLOSED_IN]->(u:AN_ASSIGNMENT_STATEMENT)-[:LOCATED_IN]-(uL:Location)
RETURN DISTINCT v, collect((u)-[]->(uL)) as WRITE_VARIABLE, l as VARIABLE_LOCATION;

// We can create kinds like this:
//MATCH (e:Test)
//  WHERE any(k in e.kind where k in ['AN_INTEGER_LITERAL', 'AN_ELEMENT'])
//RETURN e

/*
 * Match a part of write variables
 * Write in variable declaration
 */
MATCH (l:Location)<-[:LOCATED_IN]-(v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(expr:AN_EXPRESSION)
RETURN v, l as VARIABLE_LOCATION
ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;

/*
 * Assignment
 */
//MATCH (l:Location)<-[:LOCATED_IN]-(v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(expr:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(assignmentIdentifier:AN_IDENTIFIER)-[:IS_ENCLOSED_IN *1..]->(assignmentStatement:AN_ASSIGNMENT_STATEMENT)-[:LOCATED_IN]->(assignmentLocation:Location),
//(assignmentIdentifier)-[:LOCATED_IN]->(assignmentIdentifierLocation:Location)
// WHERE assignmentIdentifierLocation.filename = assignmentLocation.filename AND assignmentIdentifierLocation.line <= assignmentLocation.line AND assignmentIdentifierLocation.column <= assignmentLocation.column
//RETURN v, l as VARIABLE_LOCATION
//ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;

MATCH (l:Location)<-[:LOCATED_IN]-(v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(expr:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(assignmentIdentifier:AN_IDENTIFIER)-[:IS_ENCLOSED_IN { is_assigned_variable: true  }]->(assignmentStatement:AN_ASSIGNMENT_STATEMENT)
RETURN v, l as VARIABLE_LOCATION
  ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column
UNION
MATCH (l:Location)<-[:LOCATED_IN]-(v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(expr:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(assignmentIdentifier:AN_IDENTIFIER)-[:IS_ENCLOSED_IN *]->()-[:IS_ENCLOSED_IN { is_assigned_variable: true  }]->(assignmentStatement:AN_ASSIGNMENT_STATEMENT)
RETURN v, l as VARIABLE_LOCATION
  ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;

// Match use of generic variables
// MATCH (l:Location)<-[:LOCATED_IN]-(v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(expr:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(assignmentIdentifier:AN_IDENTIFIER)-[:IS_ENCLOSED_IN *1..]->(assignmentStatement:AN_ASSIGNMENT_STATEMENT)-[:LOCATED_IN]->(assignmentLocation:Location),
// (assignmentIdentifier)-[:LOCATED_IN]->(assignmentIdentifierLocation:Location)
// WHERE assignmentIdentifierLocation.filename = assignmentLocation.filename AND assignmentIdentifierLocation.line <= assignmentLocation.line AND assignmentIdentifierLocation.column <= assignmentLocation.column
// RETURN v, l as VARIABLE_LOCATION
// ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;

MATCH (v:A_VARIABLE_DECLARATION)-[:IS_ENCLOSED_IN]->(:A_GENERIC_PACKAGE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:IS_ENCLOSED_IN]->(:A_FORMAL_PACKAGE_DECLARATION)<-[:IS_PART_OF_INSTANCE_OF]-(assignmentIdentifier:AN_IDENTIFIER)-[:IS_ENCLOSED_IN { is_assigned_variable: true  }]->(assignmentStatement:AN_ASSIGNMENT_STATEMENT)
MATCH (p)-[:LOCATED_IN]->(l:Location)
RETURN p, l as VARIABLE_LOCATION, v
  ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column
UNION
MATCH (v:A_VARIABLE_DECLARATION)-[:IS_ENCLOSED_IN]->(:A_GENERIC_PACKAGE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:IS_ENCLOSED_IN]->(p:A_FORMAL_PACKAGE_DECLARATION)<-[:IS_PART_OF_INSTANCE_OF]-(assignmentIdentifier:AN_IDENTIFIER)-[:IS_ENCLOSED_IN *]->()-[:IS_ENCLOSED_IN { is_assigned_variable: true  }]->(assignmentStatement:AN_ASSIGNMENT_STATEMENT)
MATCH (p)-[:LOCATED_IN]->(l:Location)
RETURN p, l as VARIABLE_LOCATION, v
  ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;

// Rewrite of a part of the previous request, with the new structure of the graph
MATCH
// Une variable définit dans un package générique
  (v:A_VARIABLE_DECLARATION)-[:IS_ENCLOSED_IN]->(:A_GENERIC_PACKAGE_DECLARATION)
  // Ce package générique est instancié dans un paramètre formel
    <-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:IS_ENCLOSED_IN]->(p:A_FORMAL_PACKAGE_DECLARATION)
  // Une des variables du package formel est assigné
    <-[:CORRESPONDING_INSTANCIATION]-(assignmentIdentifier:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(assignmentStatement:AN_ASSIGNMENT_STATEMENT)
MATCH (p)-[:LOCATED_IN]->(l:Location)
RETURN p, l as VARIABLE_LOCATION, v
  ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;

//////////

// Match only user defined compilation unit
MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
RETURN cu;

// Match ALL variables defined in compilation unit defined by the user
MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)
  WHERE (v)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (l:Location)<-[:LOCATED_IN]-(v)
RETURN cu as Compilation_Unit, l as Location, v as Variable;

// Match write variable.
// This is the case where write is in the package body of the generic package and we match instactiation of this generic package.
MATCH (v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(i:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT)
MATCH (v)-[:IS_ENCLOSED_IN]->()-[:IS_ENCLOSED_IN]-(:A_GENERIC_PACKAGE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:Public_Part)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:IS_ENCLOSED_IN]->(p)
MATCH (l:Location)<-[:LOCATED_IN]-(p)
RETURN p as elt, l as VARIABLE_LOCATION
  ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;

///

// Match assigned variables
// WORKING !

MATCH (l:Location)<-[:LOCATED_IN]-(v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT)
RETURN v as elt, l as VARIABLE_LOCATION, FALSE AS isRead, TRUE AS isWrite
  ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;

MATCH (v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(i:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT)
MATCH (i)-[:CORRESPONDING_INSTANCIATION]->(p)
MATCH (l:Location)<-[:LOCATED_IN]-(p)
RETURN p as elt, l as VARIABLE_LOCATION, FALSE AS isRead, TRUE AS isWrite
  ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;

// Match assignation in generic instanciation
// WORKING !
MATCH (v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(i:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT)
MATCH (v)-[:IS_ENCLOSED_IN]->()-[:IS_ENCLOSED_IN]-(:A_GENERIC_PACKAGE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:Public_Part)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:IS_ENCLOSED_IN]->(p)
MATCH (l:Location)<-[:LOCATED_IN]-(p)
RETURN p as elt, l as VARIABLE_LOCATION, FALSE AS isRead, TRUE AS isWrite
  ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;

// Match variables usage from sub-program call
// WORKING !

MATCH (:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(i:AN_IDENTIFIER)<-[:CORRESPONDING_ACTUAL_PARAMETER]-(p:A_PARAMETER_ASSOCIATION)-[:IS_ENCLOSED_IN]->(:User_Parameters)
MATCH (v)-[:LOCATED_IN]->(l:Location)
MATCH (p)-[:CORRESPONDING_PARAMETER_SPECIFICATION]->(s:A_PARAMETER_SPECIFICATION)
RETURN p AS elt, l AS VARIABLE_LOCATION,
   CASE
     WHEN "A_DEFAULT_IN_MODE" IN labels(s) OR "AN_IN_MODE" IN labels(s) OR "AN_IN_OUT_MODE" IN labels(s) THEN TRUE
     ELSE FALSE
   END AS isRead,
   CASE
     WHEN "AN_OUT_MODE" IN labels(s) OR "AN_IN_OUT_MODE" IN labels(s) THEN TRUE
     ELSE FALSE
   END AS isWrite
  ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;

/// Try to provide an aggregated request
// Shorted version
// First, match nodes
MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)
  WHERE (v)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (l:Location)<-[:LOCATED_IN]-(v)
// Return the result of this amazing query: tell me if a variable is read or write !
RETURN cu as Compilation_Unit, l as Location, v as Variable, (FALSE) OR (
// Simple variable assignation
EXISTS((v)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT))
)AS isWrite, FALSE as isRead;

// Version with call
// First, match nodes
MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)
  WHERE (v)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (l:Location)<-[:LOCATED_IN]-(v)
// Find written variables
CALL {
  return FALSE AS isWrite1
}
CALL {
  WITH v
 return EXISTS((v)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT)) AS isWrite2
}
// Return the result of this amazing query: tell me if a variable is read or write !
RETURN cu as Compilation_Unit, l as Location, v as Variable, isWrite1 or isWrite2 AS isWrite, FALSE as isRead

UNION
MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)
  WHERE (v)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (l:Location)<-[:LOCATED_IN]-(v)
// Find written variables
CALL {
return FALSE AS isWrite1
}
CALL {
WITH v
MATCH (v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(i:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT)
return EXISTS((i)-[:CORRESPONDING_INSTANCIATION]->(p)-[:LOCATED_IN]->(l:Location)) AS isWrite2
}
// Return the result of this amazing query: tell me if a variable is read or write !
RETURN cu as Compilation_Unit, l as Location, v as Variable, isWrite1 or isWrite2 AS isWrite, FALSE as isRead;

// Get instancied generics

MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)
  WHERE (v)-[:IS_ENCLOSED_IN*]->(cu)
MATCH(v)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:CORRESPONDING_INSTANCIATION]->(pac:A_PACKAGE_INSTANTIATION)-[:LOCATED_IN]->(l:Location)
// Find written variables
CALL {
return FALSE AS isWrite1
}
CALL {
WITH v
return EXISTS((v)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:CORRESPONDING_ASSIGNATION]->(:AN_ASSIGNMENT_STATEMENT)) AS isWrite2
}
// Return the result of this amazing query: tell me if a variable is read or write !
RETURN cu as Compilation_Unit, l as Location, v as Variable, isWrite1 or isWrite2 AS isWrite, FALSE as isRead
