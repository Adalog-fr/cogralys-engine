MATCH (p)<-[r:IS_ENCLOSED_IN]-(e:Element)
  WHERE r.index IS NULL
WITH p, e, r
  ORDER BY e.filename, e.line, e.column
CALL {
WITH p
MATCH (p)<-[r2:IS_ENCLOSED_IN]-(:Element)
  WHERE r2.index IS NOT NULL
RETURN count(r2) as nb
}
WITH p, nb, collect(distinct r) as rels
FOREACH (n IN rels | SET n.index = apoc.coll.indexOf(rels, n) + nb + 1);