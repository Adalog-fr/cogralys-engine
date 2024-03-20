MATCH (e:A_DERIVED_RECORD_EXTENSION_DEFINITION|AN_INTERFACE_TYPE_DEFINITION|A_DERIVED_TYPE_DEFINITION|A_FORMAL_DERIVED_TYPE_DEFINITION|A_FORMAL_INTERFACE_TYPE_DEFINITION|A_PROTECTED_DEFINITION|A_TASK_DEFINITION|A_PRIVATE_EXTENSION_DEFINITION)
MATCH (enclosingE)-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(e)

OPTIONAL MATCH (e)<-[:IS_ENCLOSED_IN*]-(:AN_IDENTIFIER)-[:CORRESPONDING_NAME_DEFINITION]->()-[:IS_ENCLOSED_IN]->()-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(parent)
OPTIONAL MATCH (enclosingParent)-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(parent)

CALL apoc.do.when(
  parent IS NOT NULL AND 'A_SUBTYPE_INDICATION' IN labels(parent),
  'MATCH (enclosingParent)-[:CORRESPONDING_FIRST_SUBTYPE]->(realParent)-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(realTypeDecl)
   CALL apoc.do.when(realParent IS NOT NULL AND \'AN_INTERFACE_TYPE_DEFINITION\' IN labels(realTypeDecl), 
     "CALL apoc.create.relationship(enclosingParent,\\\"IS_PROGENITOR_OF\\\",{},enclosingE) YIELD rel AS relParent RETURN relParent, enclosingParent", 
     "CALL apoc.create.relationship(enclosingParent,\\\"IS_ANCESTOR_OF\\\",{},enclosingE) YIELD rel AS relParent RETURN relParent, enclosingParent", 
     {enclosingE:enclosingE,enclosingParent:realParent, realTypeDecl:realTypeDecl}) yield value as ancestors RETURN ancestors', 
  'RETURN NULL',
  {enclosingE:enclosingE,enclosingParent:enclosingParent}) yield value as ancestors

CALL apoc.do.when(
  parent IS NOT NULL AND 'AN_INTERFACE_TYPE_DEFINITION' IN labels(parent),
  'CALL apoc.create.relationship(enclosingParent,"IS_PROGENITOR_OF",{},enclosingE) YIELD rel AS relParent RETURN relParent, enclosingParent', 
  'RETURN NULL',
  {enclosingE:enclosingE, enclosingParent:enclosingParent}) yield value as interfaces

CALL apoc.do.when(
  parent IS NOT NULL AND none(label in labels(parent) WHERE label IN ['AN_INTERFACE_TYPE_DEFINITION', 'A_SUBTYPE_INDICATION']),
  'CALL apoc.create.relationship(enclosingParent,"IS_ANCESTOR_OF",{},enclosingE) YIELD rel AS relParent RETURN relParent, enclosingParent',
  "RETURN NULL",
  {enclosingE:enclosingE,enclosingParent:enclosingParent}) yield value as ancestor

RETURN enclosingE,
       ancestors,
       interfaces,
       ancestor
ORDER BY enclosingE.filename, enclosingE.line, enclosingE.column
