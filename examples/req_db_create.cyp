// Match node that have a parent and not index on there relations
// Order by filename, line, column
MATCH (p)<-[r:IS_ENCLOSED_IN]-(e:Element)-[:LOCATED_IN]->(l:Location)
  WHERE NOT exists(r.index)
WITH e, r, l
  ORDER BY l.filename, l.line, l.column
RETURN collect(e);

//
MATCH (e:Private_Part)-[:LOCATED_IN]->(l:Location)
MATCH (e)<-[:IS_ENCLOSED_IN*0..]-(c:Element)
RETURN e, l, count (c) AS NB, count (c) + 1 AS Idx

//WITH collect(p) as a
//UNWIND a as elt
//WITH DISTINCT elt
//RETURN elt.e.content, elt.l.filename, elt.l.line, elt.l.column;

//FOREACH (n IN nodes(p) | SET n.index)

MATCH (p)<-[r:IS_ENCLOSED_IN]-(e:Element)-[:LOCATED_IN]->(l:Location)
  WHERE NOT exists(r.index)
WITH p, e, r, l
  ORDER BY l.filename, l.line, l.column
OPTIONAL MATCH (p)<-[r2:IS_ENCLOSED_IN]-(:Element)
  WHERE exists(r2.INDEX)
WITH r, count(r2) as nb
SET r.index = nb + 1;

// Delete index
MATCH (p)<-[r:IS_ENCLOSED_IN]-(e:Element)
REMOVE r.index;

// Create index property on  IS_ENCLOSED_IN which provide the order of nodes in the AST
MATCH (p)<-[r:IS_ENCLOSED_IN]-(e:Element)-[:LOCATED_IN]->(l:Location)
  WHERE NOT exists(r.index)
WITH p, e, r, l
  ORDER BY l.filename, l.line, l.column
CALL {
WITH p
MATCH (p)<-[r2:IS_ENCLOSED_IN]-(:Element)
  WHERE exists(r2.index)
RETURN count(r2) as nb
}
SET r.index = nb + 1;
