// Init, find element
MATCH (cu:Compilation_Unit { is_predefined_unit: false })
  WHERE NOT cu.content STARTS WITH "System"
MATCH (:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(v:A_DEFINING_IDENTIFIER)
MATCH p = (v)-[:IS_ENCLOSED_IN*]->(cu)
MATCH (v)-[:LOCATED_IN]->(l:Location { line: 20, column: 7 })
// Reconstruct path

WITH reduce(output = [], n IN nodes(p) | output + n ) as nodeCollection, v
// here we take the single row of the flattened array and pivot it to rows.
UNWIND nodeCollection[2..] as ancestorNode
MATCH (ancestorNode)
  WHERE "A_DECLARATION" in labels(ancestorNode)
MATCH (r:A_DEFINING_IDENTIFIER)
  WHERE (ancestorNode)<-[:IS_ENCLOSED_IN]-(r) OR (ancestorNode)<-[:IS_ENCLOSED_IN]-(:Public_Part)<-[:IS_ENCLOSED_IN]-(r:A_DEFINING_IDENTIFIER)
WITH v, collect(distinct r.content) as a
RETURN v as variable, REDUCE(acc=HEAD(a), s in TAIL(a) | s + "." + acc ) + "." + v.content as ancestors
