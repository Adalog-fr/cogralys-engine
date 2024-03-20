MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (vD:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
  WHERE (vD)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (v)-[:LOCATED_IN]->(l:Location)

WITH *,
  CASE WHEN size([(v)-[:IS_ENCLOSED_IN*]->(decl:A_GENERIC_PACKAGE_DECLARATION) | decl]) = 0 THEN v ELSE null END AS normalVar,
  CASE WHEN size([(v)-[:IS_ENCLOSED_IN*]->(decl:A_GENERIC_PACKAGE_DECLARATION) | decl]) > 0 THEN v ELSE null END AS genericVar
WITH collect(normalVar) as varsN, collect(genericVar) as varsG

return varsN, varsG
