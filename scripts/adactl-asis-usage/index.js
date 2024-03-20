const { basename, join } = require("path");
require("dotenv").config({ path: join(__dirname, ".env") });
const Table = require("tty-table");
const { readFileSync, writeFileSync } = require("fs");
const axios = require("axios");

const query = readFileSync(join(__dirname, "adactl-asis-usage.cyp"), { encoding: "utf8", flag: "r" });

console.time("queryTime");
axios
    .post(
        process.env.NEO4J_HOST,
        {
            statements: [
                {
                    statement: query,
                },
            ],
        },
        {
            headers: {
                "Content-Type": "application/json",
            },
            auth: {
                username: process.env.NEO4J_USER,
                password: process.env.NEO4J_PASS,
            },
        }
    )
    .then((res) => {
        console.timeEnd("queryTime");
        if (res.data.errors.length) {
            for (const elt of res.data.errors) {
                console.error("error: ", elt);
            }
        } else {
            // console.log("res.data.results: ", res.data.results);
            writeFileSync(join(__dirname, "result-direct.json"), JSON.stringify(res.data.results[0].data, null, 2));
            // processResult([...res.data.results[0].data, ...res.data.results[1].data].map(elt => elt.row));
        }
    })
    .catch((e) => {
        console.error("error: ", e);
    });
