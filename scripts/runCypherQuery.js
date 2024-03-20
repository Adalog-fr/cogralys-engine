const { join } = require("path");
require("dotenv").config({ path: join(__dirname, ".env") });
const { readFileSync, writeFileSync } = require("fs");
const axios = require("axios");
const { Command } = require("commander");
const program = new Command();

program
    .name("runCypherQuery")
    .description("Run a cypher query file, and put the result into a JSON file")
    .argument("<inputFile>", "path to cypher file")
    .argument("<outputFile>", "path to output JSON file");

const result = program.parse(process.argv);

const [inputFile, outputFile] = result.args;


const query = readFileSync(inputFile, { encoding: "utf8", flag: "r" });

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
            writeFileSync(outputFile, JSON.stringify(res.data.results[0].data, null, 2));
        }
    })
    .catch((e) => {
        console.error("error: ", e);
    });
