require("dotenv").config();
const Table = require('tty-table');
const { basename, join } = require("path");
const { readFileSync } = require("fs");
const axios = require("axios");
const ref = require("./adactl_result.json");

const data = readFileSync(join(__dirname, "queries", "get_variables_usage_by_all_categories.cyp"), {encoding:'utf8', flag:'r'});
// const data = readFileSync(join(__dirname, "queries", "get_variables_usage_by_categories.cyp"), {encoding:'utf8', flag:'r'});
// const data = readFileSync(join(__dirname, "variable_usage.cyp"), {encoding:'utf8', flag:'r'});
// const data = readFileSync(join(__dirname, "variable_usage_old.cyp"), {encoding:'utf8', flag:'r'});

console.time('queryTime');
axios.post(process.env.NEO4J_HOST, {
    "statements": [
        {
            "statement": data
        }
    ]
}, {
    headers: {
        "Content-Type": "application/json"
    },
    auth: {
        username: process.env.NEO4J_USER,
        password: process.env.NEO4J_PASS
    }
}).then(res => {
    console.timeEnd('queryTime');
    if (res.data.errors.length) {
        for (const elt of res.data.errors) {
            console.error("error: ", elt);
        }
    } else {
        processResult(res.data.results[0].data.map(elt => elt.row));
    }
}).catch(e => {
    console.error("error: ", e);
});

function processResult(resultFromNeo4J) {
    // Initialize values
    for (const elt of ref) {
        elt.matched = false;
    }

    let notFoundInAdactl = [];

    for (const record of resultFromNeo4J) {
        const filename = basename(record[1].filename);
        const line = record[1].line;
        const column = record[1].column;
        const content = record[2].content;

        const r = ref.find(e => (e.filename === filename && e.line === line && e.column === column && e.content.endsWith(content)));
        if (r) {
            r.matched = true;
            r.is_written_actual = r.is_written_actual || record[3];
            r.is_read_actual = r.is_read_actual || record[4];
        } else {
            notFoundInAdactl.push({
                filename,
                line,
                column,
                content,
            })
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
        { value: "Origin", headerColor: "cyan" },
        {
            value: "Found by Neo4J ?",
            headerColor: "cyan",
            formatter(value) {
                return value ? this.style("Yes", "green", "bold") : this.style("No", "bgRed", "white")
            }
        },
        {
            value: "Is written ?",
            headerColor: "cyan",
            formatter(object) {
                if (object.expected === true) {
                    return object.expected === object.actual ? this.style("Yes") : this.style("<span class=\"text-red-500\">Yes</span>", "bgRed", "white")
                } else if (object.expected === false) {
                    return object.expected === object.actual ? this.style("No") : this.style("<span class=\"text-red-500\">No</span>", "bgRed", "white")
                }
                return value ? this.style("Yes", "green", "bold") : this.style("No", "bgRed", "white")
            }
        },
        {
            value: "Is read ?",
            headerColor: "cyan",
            formatter(object) {
                if (object.expected === true) {
                    return object.expected === object.actual ? this.style("Yes") : this.style("Yes", "bgRed", "white")
                } else if (object.expected === false) {
                    return object.expected === object.actual ? this.style("No") : this.style("No", "bgRed", "white")
                }
                return value ? this.style("Yes", "green", "bold") : this.style("No", "bgRed", "white")
            }
        },
        { value: "Note", headerColor: "cyan" },
    ];

    const rows = [];

    for (const elt of ref) {
        rows.push([
            elt.filename,
            elt.line,
            elt.column,
            elt.content,
            elt.origin,
            // Found by Neo4J?
            elt.matched,
            // Is Written?
            { expected: elt.is_written, actual: elt.is_written_actual, ctx: elt },
            // Is read?
            { expected: elt.is_read, actual: elt.is_read_actual, ctx: elt },
            // Note
            elt.note
        ]);
    }


    console.log(Table(header, rows).render())

    console.log("Total elt in Adactl: ", ref.length);
    console.log("Total of Adactl found by Neo4j: ", ref.filter(e => e.matched).length);
    console.log("Total of Adactl not found by Neo4j: ", ref.filter(e => !e.matched).length);
    console.log("Total of written variables found by Adactl: ", ref.filter(e => e.is_written).length);
    console.log("Total of read variables found by Adactl: ", ref.filter(e => e.is_read).length);
    console.log("Total of Neo4j not found in Adatctl: ", notFoundInAdactl.length);
}
