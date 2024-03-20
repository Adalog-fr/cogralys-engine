const Table = require('tty-table');
const { basename } = require("path");

const ref = require("./adactl_result.json");
const response = require("./write_var_responde.json");

// MATCH (l:Location)<-[:LOCATED_IN]-(v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(expr:AN_EXPRESSION)
// RETURN v, l as VARIABLE_LOCATION
// ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;
const response2 = require("./var_decl_write_response.json");

// MATCH (l:Location)<-[:LOCATED_IN]-(v:A_VARIABLE_DECLARATION)<-[:IS_ENCLOSED_IN]-(expr:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(assignmentIdentifier:AN_IDENTIFIER)-[:IS_ENCLOSED_IN *1..]->(assignmentStatement:AN_ASSIGNMENT_STATEMENT)-[:LOCATED_IN]->(assignmentLocation:Location),
// (assignmentIdentifier)-[:LOCATED_IN]->(assignmentIdentifierLocation:Location)
// WHERE assignmentIdentifierLocation.filename = assignmentLocation.filename AND assignmentIdentifierLocation.line <= assignmentLocation.line AND assignmentIdentifierLocation.column <= assignmentLocation.column
// RETURN v, l as VARIABLE_LOCATION
// ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;
const response3 = require("./assignment_response.json");

// MATCH (v:A_VARIABLE_DECLARATION)-[:IS_ENCLOSED_IN]->(:A_GENERIC_PACKAGE_DECLARATION)<-[:IS_ENCLOSED_IN]-(:A_DEFINING_IDENTIFIER)<-[:CORRESPONDING_NAME_DEFINITION]-(:AN_IDENTIFIER)-[:IS_ENCLOSED_IN]->(p:A_FORMAL_PACKAGE_DECLARATION)<-[:IS_PART_OF_INSTANCE_OF]-(assignmentIdentifier:AN_IDENTIFIER)-[:IS_ENCLOSED_IN *]->()-[:IS_ENCLOSED_IN { is_assigned_variable: true  }]->(assignmentStatement:AN_ASSIGNMENT_STATEMENT)
// MATCH (p)-[:LOCATED_IN]->(l:Location)
// RETURN p, l as VARIABLE_LOCATION, v
// ORDER BY VARIABLE_LOCATION.filename, VARIABLE_LOCATION.line, VARIABLE_LOCATION.column;
const response4 = require("./write_in_instanciation.json");

const responses = [response2, response3, response4];

// Initialize values
for (const elt of ref) {
    elt.matched = false;
}

let notFoundInAdactl = [];

for (const record of response) {
    const filename = basename(record._fields[2].properties.filename);
    const line = record._fields[2].properties.line.low;
    const column = record._fields[2].properties.column.low;
    const content = record._fields[0].properties.content;

    const r = ref.find(e => (e.filename === filename && e.line === line && e.column === column));
    if (r) {
        r.matched = true;
    } else {
        notFoundInAdactl.push({
            filename,
            line,
            column,
            content,
        })
    }
}

for (const response of responses) {
    for (const record of response) {
        const filename = basename(record._fields[1].properties.filename);
        const line = record._fields[1].properties.line.low;
        const column = record._fields[1].properties.column.low;
        const content = record._fields[0].properties.content;

        const r = ref.find(e => (e.filename === filename && e.line === line && e.column === column));
        if (r) {
            r.matched = true;
        } else {
            notFoundInAdactl.push({
                filename,
                line,
                column,
                content,
            })
        }
    }
}

if (notFoundInAdactl.length) {
    console.log("Elements not found in Adactl logs:");
    for (const elt of notFoundInAdactl) {
        console.log(`${elt.filename}:${elt.line}:${elt.column} => ${elt.content}`);
    }
}

const header = [
    { value: "Filename", headerColor: "cyan" },
    { value: "Line", headerColor: "cyan", align: "right" },
    { value: "Column", headerColor: "cyan", align: "right" },
    { value: "Content", headerColor: "cyan" },
    {
        value: "Found by Neo4J ?",
        headerColor: "cyan",
        formatter(value) {
            return value ? this.style("Yes", "green", "bold") : this.style("No", "bgRed", "white")
        }
    },
    { value: "Is written ?", headerColor: "cyan" },
    { value: "Is read ?", headerColor: "cyan" },
    { value: "Note", headerColor: "cyan" },
];

const rows = [];

for (const elt of ref) {
    rows.push([elt.filename, elt.line, elt.column, elt.content, elt.matched, elt.is_written ? "Yes" : "No", elt.is_read ? "Yes" : "No", elt.note]);
}


console.log(Table(header, rows).render())

console.log("Total elt in Adactl: ", ref.length);
console.log("Total of Adactl found by Neo4j: ", ref.filter(e => e.matched).length);
console.log("Total of Adactl not found by Neo4j: ", ref.filter(e => !e.matched).length);
console.log("Total of written variables in found by Adactl: ", ref.filter(e => e.is_written).length);
console.log("Total of read variables in found by Adactl: ", ref.filter(e => e.is_read).length);
console.log("Total of Neo4j not found in Adatctl: ", notFoundInAdactl.length);
