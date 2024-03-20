require("dotenv").config();
const Table = require('tty-table');
const { basename, join } = require("path");
const { readFileSync } = require("fs");
const axios = require("axios");

const dataNormalAndGeneric = readFileSync(join(__dirname, "queries", "variable_usage", "normal_and_generic.cyp"), {encoding:'utf8', flag:'r'});
const dataInstance = readFileSync(join(__dirname, "queries", "variable_usage", "instance.cyp"), {encoding:'utf8', flag:'r'});

console.time('queryTime');
axios.post(process.env.NEO4J_HOST, {
    "statements": [
        {
            "statement": dataNormalAndGeneric
        },
        {
            "statement": dataInstance
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
        console.log("res.data.results: ", res.data.results.length);
        processResult([...res.data.results[0].data, ...res.data.results[1].data].map(elt => elt.row));
    }
}).catch(e => {
    console.error("error: ", e);
});

function processResult(resultFromNeo4J) {
    const ref = [];

    for (const record of resultFromNeo4J) {
        const r = {
            filename : basename(record[1].filename),
            line : record[1].line,
            column : record[1].column,
            content : record[2].content,
            origin: record[5],
            is_written : record[3],
            is_read : record[4],
        };

        ref.push(r);
    }

    const header = [
        { value: "Filename", headerColor: "cyan" },
        { value: "Line", headerColor: "cyan", align: "right" },
        { value: "Column", headerColor: "cyan", align: "right" },
        { value: "Content", headerColor: "cyan" },
        { value: "Origin", headerColor: "cyan" },
        {
            value: "Is written ?",
            headerColor: "cyan",
            formatter(value) {
                return value ? "Yes" : "No"
            }
        },
        {
            value: "Is read ?",
            headerColor: "cyan",
            formatter(value) {
                return value ? "Yes" : "No"
            }
        },
    ];

    const rows = [];

    for (const elt of ref) {
        rows.push([
            elt.filename,
            elt.line,
            elt.column,
            elt.content,
            elt.origin,
            // Is Written?
            elt.is_written,
            // Is read?
            elt.is_read,
        ]);
    }


    console.log(Table(header, rows).render())

    console.log("Total elt found: ", ref.length);
}
