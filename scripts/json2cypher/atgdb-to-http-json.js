const { readFileSync, writeFileSync, accessSync, mkdirSync } = require("fs");
const { join: joinPath } = require("path");

const allContents = readFileSync("database_mid.json", "utf-8");

const basePath = "cypher_http_queries";

try {
    accessSync(joinPath(__dirname, basePath));
} catch {
    mkdirSync(joinPath(__dirname, basePath));
}

const fileDescriptors = {};

const propertyToRelationName = {
    parent: "IS_ENCLOSED_IN",
    type_declaration: "IS_OF_TYPE",
    corresponding_name_definition: "CORRESPONDING_NAME_DEFINITION",
    corresponding_instanciation: "CORRESPONDING_INSTANTIATION",
    corresponding_assignation: "CORRESPONDING_ASSIGNATION",
    corresponding_formal_name: "CORRESPONDING_FORMAL_NAME",
    corresponding_actual_parameter: "CORRESPONDING_ACTUAL_PARAMETER",
    corresponding_parameter_specification: "CORRESPONDING_PARAMETER_SPECIFICATION",
};

const data = [];
allContents.split(/\r?\n/).forEach((line) => {
    if (!line.length) {
        return;
    }
    try {
        data.push(JSON.parse(line));
    } catch (e) {
        console.error("error with:", line);
        throw e;
    }
});

function propsToCypherProps(props) {
    return JSON.stringify(props);
    let result = "{";

    for (const key in props) {
        result += key + ": " + JSON.stringify(props[key]) + ",";
    }

    if (result.length > 1) {
        result = result.slice(0, -1);
    }

    return result + "}";
}

// console.log("data: ", data);

const queryNodesByLabels = {};

const queryRelationshipsByLabels = {};

let currentId = 0;
const nodeIdToId = {};

// function getId(node_id) {
//     if (node_id in nodeIdToId) {
//         return nodeIdToId[node_id];
//     }
//     return currentId;
// }

const nodeIdList = [];

for (const node of data) {
    // Create current node

    if (nodeIdList.includes(node.node_id)) {
        console.log("node already defined: ", node);
        continue;
    }

    const currentNodeProperties = {
        node_id: node.node_id,
        filename: node.location.filename,
        line: node.location.line,
        column: node.location.column,
    };

    const addProperty = (key) => {
        if (!(key in node)) {
            return;
        }
        currentNodeProperties[key] = node[key];
    };

    addProperty("content");
    addProperty("element_kind");
    addProperty("enclosing_unit");
    addProperty("is_part_of_implicit");
    addProperty("is_part_of_inherited");
    addProperty("is_part_of_instance");
    addProperty("special_case");
    addProperty("is_default_parameter");
    addProperty("is_named_parameter_association");
    addProperty("is_predefined_unit");

    if ("kinds" in node) {
        currentNodeProperties.kinds = node.kinds;
    } else if ("kind" in node) {
        currentNodeProperties.kinds = [node.kind];
    } else {
        throw Error("the following node does not have 'kinds' or 'kind': " + JSON.stringify(node));
    }

    const labelKey = currentNodeProperties.kinds.join("+");
    if (!(labelKey in queryNodesByLabels)) {
        queryNodesByLabels[labelKey] = "";
    }

    // queryNodesByLabels[labelKey] += `{properties:${propsToCypherProps(currentNodeProperties)}},`;
    queryNodesByLabels[labelKey] += `{"_id": ${JSON.stringify(
        currentNodeProperties.node_id
    )}, "properties":${propsToCypherProps(currentNodeProperties)}},`;
    // queryNodesByLabels[key] += `{_id:${node.id}, properties:${propsToCypherProps(node.properties)}},`;
    nodeIdToId[node.node_id] = currentId;
    nodeIdList.push(node.node_id);
    // Create relationships

    for (const relationKey of Object.keys(propertyToRelationName)) {
        if (!(relationKey in node)) {
            continue;
        }

        if (!(propertyToRelationName[relationKey] in queryRelationshipsByLabels)) {
            queryRelationshipsByLabels[propertyToRelationName[relationKey]] = "";
        }

        if (!(node[relationKey].node_id in nodeIdToId)) {
            currentId++;
            nodeIdToId[node[relationKey].node_id] = currentId;
        }
        // if (queryRelationshipsByLabels[propertyToRelationName[relationKey]].length < 1000) {
        //     queryRelationshipsByLabels[
        //         propertyToRelationName[relationKey]
        //     ] += `{start:{node_id:"${currentNodeProperties.node_id}"},end:{node_id:"${node[relationKey].node_id}"},properties:{}},`;
        // }
        // queryRelationshipsByLabels[propertyToRelationName[relationKey]] += `{start:{node_id:${JSON.stringify(
        //     currentNodeProperties.node_id
        // )}},end:{node_id:${JSON.stringify(node[relationKey].node_id)}},properties:{}},`;
        queryRelationshipsByLabels[propertyToRelationName[relationKey]] += `{"start":{"_id":${JSON.stringify(
            currentNodeProperties.node_id
        )}},"end":{"_id":${JSON.stringify(node[relationKey].node_id)}},"properties":{}},`;

        // queryRelationshipsByLabels[propertyToRelationName[relationKey]] += `{start: { _id: ${getId(
        //     currentNodeProperties.node_id
        // )} }, end: { _id: ${getId(node[relationKey].node_id)}}, properties:{}},`;
    }

    currentId++;
}

// Create result

// let result = `:begin
// CREATE CONSTRAINT uniqueNodeIds IF NOT EXISTS FOR (node:Element) REQUIRE (node.node_id) IS UNIQUE;
// CREATE CONSTRAINT uniqueImportLabel IF NOT EXISTS FOR (node:\`UNIQUE IMPORT LABEL\`) REQUIRE (node.node_id) IS UNIQUE;
// :commit
// CALL db.awaitIndexes(300);
// :begin
// `;

    writeFileSync(
        joinPath(__dirname, basePath, "0_initialize.json"),
        `{
  "statements": [
    {
      "statement": "CREATE CONSTRAINT uniqueNodeIds IF NOT EXISTS FOR (node:Element) REQUIRE (node.node_id) IS UNIQUE;",
      "parameters": {}
    },
    {
      "statement": "CREATE CONSTRAINT uniqueImportLabel IF NOT EXISTS FOR (node:\`UNIQUE IMPORT LABEL\`) REQUIRE (node.node_id) IS UNIQUE;",
      "parameters": {}
    },
    {
      "statement": "CALL db.awaitIndexes(300);",
      "parameters": {}
    }
  ]
}`
    );

for (const key in queryNodesByLabels) {
    // result += `:param rows => [${queryNodesByLabels[key].slice(0, -1)}]\n`;
    // result += `UNWIND $rows AS row\n`;
    // result += `CREATE(n: \`UNIQUE IMPORT LABEL\`{node_id: row._id}) SET n += row.properties SET n:${key};\n`;

    const statement = `UNWIND $rows AS row
CREATE(n: \`UNIQUE IMPORT LABEL\`{node_id: row._id}) SET n += row.properties SET n:${key.replace(/[+]/g, ":")};`;

    writeFileSync(
        joinPath(__dirname, basePath, "1_" + key + ".json"),
        `{
  "statements": [
    {
      "statement": ${JSON.stringify(statement)},
      "parameters": {"rows":[${queryNodesByLabels[key].slice(0, -1)}]}
    }
  ]
}`
    );
}

// result += `:commit
// :begin
// `;

for (const key in queryRelationshipsByLabels) {
    // result += `:param rows => [${queryRelationshipsByLabels[key].slice(0, -1)}]\n`;
    // result += `UNWIND $rows AS row\n`;
    // result += "MATCH (start:`UNIQUE IMPORT LABEL`{node_id: row.start._id})\n";
    // result += "MATCH (end:`UNIQUE IMPORT LABEL`{node_id: row.end._id})\n";
    // // result += "MATCH (start:`UNIQUE IMPORT LABEL`{node_id: row.start.node_id})\n";
    // // result += "MATCH (end:`UNIQUE IMPORT LABEL`{node_id: row.end.node_id})\n";
    // result += `CREATE (start)-[r:${key}]->(end) SET r += row.properties;\n`;

    const statement = `UNWIND $rows AS row
MATCH (start:\`UNIQUE IMPORT LABEL\`{node_id: row.start._id})
MATCH (end:\`UNIQUE IMPORT LABEL\`{node_id: row.end._id})
CREATE (start)-[r:${key.replace(/[+]/g, ":")}]->(end) SET r += row.properties;`;

        writeFileSync(
            joinPath(__dirname, basePath, "2_" + key + ".json"),
            `{
  "statements": [
    {
      "statement": ${JSON.stringify(statement)},
      "parameters": {"rows":[${queryRelationshipsByLabels[key].slice(0, -1)}]}
    }
  ]
}`
        );
}

// result += `:commit
// :begin
// MATCH (n:\`UNIQUE IMPORT LABEL\`) REMOVE n:\`UNIQUE IMPORT LABEL\`;
// :commit
// :begin
// DROP CONSTRAINT uniqueImportLabel IF EXISTS;
// :commit
// :begin
// MATCH (p)<-[r:IS_ENCLOSED_IN]-(e:Element)
//   WHERE r.index IS NULL
// WITH p, e, r
//   ORDER BY e.filename, e.line, e.column
//   CALL {
// WITH p
// MATCH (p)<-[r2:IS_ENCLOSED_IN]-(:Element)
//   WHERE r2.index IS NOT NULL
//   RETURN count(r2) as nb
//   }
// WITH p, nb, collect(distinct r) as rels
// FOREACH (n IN rels | SET n.index = apoc.coll.indexOf(rels, n) + nb + 1);
// :commit
// `;

const lastStatement = `MATCH (p)<-[r:IS_ENCLOSED_IN]-(e:Element)
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
FOREACH (n IN rels | SET n.index = apoc.coll.indexOf(rels, n) + nb + 1);`;

    writeFileSync(
        joinPath(__dirname, basePath, "3_finalize.json"),
        `{
  "statements": [
    {
      "statement": "MATCH (n:\`UNIQUE IMPORT LABEL\`) REMOVE n:\`UNIQUE IMPORT LABEL\`;",
      "parameters": {}
    },
    {
      "statement": "DROP CONSTRAINT uniqueImportLabel IF EXISTS;",
      "parameters": {}
    },
    {
      "statement": ${JSON.stringify(lastStatement)},
      "parameters": {}
    }
  ]
}`
    );

// writeFileSync("database_mid.cypher", result);

console.log("currentId: ", currentId);
