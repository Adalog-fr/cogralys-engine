const { readFileSync, writeFileSync } = require("fs");

const allContents = readFileSync("db.json", "utf-8");

const data = [];
allContents.split(/\r?\n/).forEach((line) => {
    data.push(JSON.parse(line));
});


// writeFileSync("db_json.json", JSON.stringify(data, null, 2));
// console.log("data: ", data);

function propsToCypherProps(props) {
    let result = "{"

    for (const key in props) {
        result += key + ": " + JSON.stringify(props[key]) + ",";
    }

    if (result.length > 1) {
        result = result.slice(0, -1);
    }

    return result + "}";
}

const nodes = data.filter((elt) => elt.type === "node");
const relationships = data.filter((elt) => elt.type === "relationship");

console.log("nodes: ", nodes.length);
console.log("relationships: ", relationships.length);

const queryNodesByLabels = {};

for (const node of nodes) {
    let labels = node.labels;
    try {
        labels.sort();
    } catch (e) {
        console.log("error with node: ", node);
        throw e;
    }
    const key = labels.join(":");

    if (!(key in queryNodesByLabels)) {
        queryNodesByLabels[key] = "";
    }

    queryNodesByLabels[key] += `{"_id":${node.id}, "properties":${propsToCypherProps(node.properties)}},`;
}

let result = `:begin
CREATE INDEX nodeIdIndex IF NOT EXISTS FOR (node:Element) ON (node.node_id);
CREATE CONSTRAINT uniqueImportLabel IF NOT EXISTS FOR (node:\`UNIQUE IMPORT LABEL\`) REQUIRE (node.\`UNIQUE IMPORT ID\`) IS UNIQUE;
:commit
CALL db.awaitIndexes(300);
:begin
`;

for (const key in queryNodesByLabels) {
    result += `:param rows => [${queryNodesByLabels[key].slice(0, -1)}]\n`;
    result += `UNWIND $rows AS row\n`;
    result += `CREATE(n: \`UNIQUE IMPORT LABEL\`{\`UNIQUE IMPORT ID\`: row._id}) SET n += row.properties SET n:${key};\n`;
}

result += `:commit
:begin
`;


const queryRelationshipsByLabels = {};

for (const relationship of relationships) {
    if (!(relationship.label in queryRelationshipsByLabels)) {
        queryRelationshipsByLabels[relationship.label] = "";
    }

    queryRelationshipsByLabels[relationship.label] += `{"start": {"_id": ${relationship.start.id}}, "end": {"_id": ${
        relationship.end.id
    }}, "properties":${relationship.properties ? propsToCypherProps(relationship.properties) : "{}"}},`;
}

for (const key in queryRelationshipsByLabels) {
    result += `:param rows => [${queryRelationshipsByLabels[key].slice(0, -1)}]\n`;
    result += `UNWIND $rows AS row\n`;
    result += "MATCH (start:`UNIQUE IMPORT LABEL`{`UNIQUE IMPORT ID`: row.start._id})\n";
    result += "MATCH (end:`UNIQUE IMPORT LABEL`{`UNIQUE IMPORT ID`: row.end._id})\n";
    result += `CREATE (start)-[r:${key.replace(/+/g, ":")}]->(end) SET r += row.properties;\n`;
}

result += `:commit
:begin
MATCH (n:\`UNIQUE IMPORT LABEL\`)  WITH n LIMIT 20000 REMOVE n:\`UNIQUE IMPORT LABEL\` REMOVE n.\`UNIQUE IMPORT ID\`;
:commit
:begin
DROP CONSTRAINT uniqueImportLabel IF EXISTS;
:commit
`;

// TODO: fix la suppression de la contrainte suivante dans le code au dessus.
// DROP CONSTRAINT ON (node:\`UNIQUE IMPORT LABEL\`) ASSERT (node.\`UNIQUE IMPORT ID\`) IS UNIQUE;

writeFileSync("db_json.cypher", result);
