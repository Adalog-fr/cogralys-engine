with GNATCOLL.JSON;

package neo4j_wrapper is
   use GNATCOLL.JSON;

   type Props_Kind is (PK_Boolean, PK_Integer);

   procedure Add (Element : JSON_Value);
   procedure Finalize;
end neo4j_wrapper;
