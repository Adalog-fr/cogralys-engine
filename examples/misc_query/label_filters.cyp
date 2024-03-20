// Find any instantiation of generics and get their corresponding generics

// This query is not efficient because it scan all nodes before applying the label filter
MATCH (inst)<-[:IS_ENCLOSED_IN { index: 2 }]-(gen)-[:CORRESPONDING_NAME_DEFINITION]->(genPack)
  WHERE any (x IN labels(inst) WHERE x IN
  ['A_FORMAL_PACKAGE_DECLARATION','A_FORMAL_PACKAGE_DECLARATION_WITH_BOX','A_PACKAGE_INSTANTIATION'])
RETURN *


// This is the most efficient approach
/*
 * This will perform a label scan for the labels A_FORMAL_PACKAGE_DECLARATION, A_FORMAL_PACKAGE_DECLARATION_WITH_BOX,
 * A_PACKAGE_INSTANTIATION, instead of all node scan which happens with the previous solution.
 */
MATCH (inst)<-[:IS_ENCLOSED_IN { index: 2 }]-(gen)-[:CORRESPONDING_NAME_DEFINITION]->(genPack)
  WHERE (inst:A_FORMAL_PACKAGE_DECLARATION) OR (inst:A_FORMAL_PACKAGE_DECLARATION_WITH_BOX) OR (inst:A_PACKAGE_INSTANTIATION)
RETURN *
