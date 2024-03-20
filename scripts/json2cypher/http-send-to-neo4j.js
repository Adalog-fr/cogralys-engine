const { readFileSync, readdirSync } = require("fs");
const { join: joinPath } = require("path");
const axios = require("axios").default;

const basePath = "cypher_http_queries";

// const testFilename = "0_initialize.json";
// const testFilename = "1_Element+A_PATH+AN_IF_EXPRESSION_PATH.json";

// const data = JSON.parse(readFileSync(joinPath(__dirname, basePath, testFilename)));

let files;

try {
    files = readdirSync(joinPath(__dirname, basePath));
} catch (err) {
    console.log(err);
}

const instance = axios.create({
    baseURL: "http://127.0.0.1:7474/",
    auth: {
        username: "neo4j",
        password: "auieauie",
    },
});

let initPath;
let nodesPath = [];
let edgesPath = [];
let finPath = [];

async function main() {
    console.log("Start main");

    try {
        const r = await instance({
            url: "/db/neo4j/tx/commit",
            data: {
                statements: [
                    {
                        statement:
                            "CREATE CONSTRAINT uniqueNodeIds IF NOT EXISTS FOR (node:Element) REQUIRE (node.node_id) IS UNIQUE;",
                    },
                    {
                        statement:
                            "CREATE CONSTRAINT uniqueImportLabel IF NOT EXISTS FOR (node:`UNIQUE IMPORT LABEL`) REQUIRE (node.node_id) IS UNIQUE;",
                    },
                    // { statement: "CALL db.awaitIndexes(300);" },
                ],
            },
            method: "POST",
            headers: {
                "Content-Type": "application/json",
                Accept: "application/json",
            },
        });
        if (r.errors) {
            console.error(r);
            throw Error("error on request");
        }
    } catch (e) {
        console.error("error on add nodes: ", e);
    }

    // return;

    // try {
    //     const r = await instance({
    //         url: "/db/neo4j/tx/commit",
    //         data: readFileSync(initPath),
    //         method: "POST",
    //         headers: {
    //             "Content-Type": "application/json",
    //             Accept: "application/json",
    //         },
    //     });
    //     if (r.errors) {
    //         console.error(r);
    //         throw Error("error on request");
    //     }
    // } catch (e) {
    //     console.error("error on add nodes: ", e);
    // }

    console.log("after init");

    let nodesPromises = [];

    for (const path of nodesPath) {
        console.log("send:", path);
        nodesPromises.push(
            instance({
                url: "/db/neo4j/tx/commit",
                data: readFileSync(path),
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                    Accept: "application/json",
                },
            })
        );
    }

    try {
        await Promise.all(nodesPromises);
        console.log("nodes added");
    } catch (e) {
        console.error("error on add nodes: ", e);
    }

    let edgesPromises = [];

    for (const path of edgesPath) {
        console.log("send:", path);
        edgesPromises.push(
            instance({
                url: "/db/neo4j/tx/commit",
                data: readFileSync(path),
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                    Accept: "application/json",
                },
            })
        );
    }

    try {
        await Promise.all(edgesPromises);
        console.log("edges added");
    } catch (e) {
        console.error("error on add edges: ", e);
    }

    for (const path of finPath) {
        console.log("send:", path);

        try {
            const r = await instance({
                url: "/db/neo4j/tx/commit",
                data: readFileSync(path),
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                    Accept: "application/json",
                },
            });
            if (r.errors) {
                console.error(r);
                throw Error("error on request");
            }
        } catch (e) {
            console.error("error on add nodes: ", e);
        }
    }

    console.log("end main");
}

for (const file of files) {
    if (file.endsWith(".json")) {
        if (file.startsWith("0_")) {
            initPath = joinPath(__dirname, basePath, file);
        } else if (file.startsWith("3_")) {
            finPath.push(joinPath(__dirname, basePath, file));
        } else if (file.startsWith("1_")) {
            nodesPath.push(joinPath(__dirname, basePath, file));
        } else if (file.startsWith("2_")) {
            edgesPath.push(joinPath(__dirname, basePath, file));
        }
    }
}

// console.log(initPath);
// console.log(nodesPath);
// console.log(edgesPath);
// console.log(finPath);

main();

// console.log(data.statements[0].parameters);
