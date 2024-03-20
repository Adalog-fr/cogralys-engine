// First, we get all working nodes
MATCH (integerKindDef)
WHERE "A_SIGNED_INTEGER_TYPE_DEFINITION" IN labels(integerKindDef) OR "A_MODULAR_TYPE_DEFINITION" IN labels(integerKindDef)
OPTIONAL MATCH (theType)-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(derivedType:A_DERIVED_TYPE_DEFINITION)-[:CORRESPONDING_ROOT_TYPE]->()-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(trueType)
WHERE "A_SIGNED_INTEGER_TYPE_DEFINITION" IN labels(trueType) OR "A_MODULAR_TYPE_DEFINITION" IN labels(trueType)
WITH collect(integerKindDef) as allIntegerKindDef, collect(theType) as allTheTypes
WITH apoc.coll.union(allIntegerKindDef, allTheTypes) as elements
RETURN elements