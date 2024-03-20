// The goal of this query is to understand how UNWIND work in a specific case:
with [{ decl: "decl_A", vars: ["decl_A vars_1", "decl_A vars_2", "decl_A vars_3"], instances: ["decl_A instances_1", "decl_A instances_2", "decl_A instances_3"] },{ decl: "decl_B", vars: ["decl_B vars_1", "decl_B vars_2", "decl_B vars_3"], instances: ["decl_B instances_1", "decl_B instances_2", "decl_B instances_3"] },{ decl: "decl_C", vars: ["decl_C vars_1", "decl_C vars_2", "decl_C vars_3"], instances: ["decl_C instances_1", "decl_C instances_2", "decl_C instances_3"] }] as genPackMapDummy
unwind genPackMapDummy as genPackMap

CALL {
    with genPackMap
    WITH genPackMap.instances as instances, genPackMap.vars as vars
    UNWIND instances AS instance

    CALL {
            WITH vars, instance
            UNWIND vars AS var
            RETURN instance + " >> " + var as Concat
    }
    return Concat
}

RETURN Concat
