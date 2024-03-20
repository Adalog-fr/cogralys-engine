require("dotenv").config();

const axios = require("axios");
const req = require("../../examples/adactl/wks/req.json");

console.time('codezup');

async function main() {
    console.time('codezup');
    for (const data of req) {
        try {
            await axios.post(process.env.NEO4J_HOST, data, {
                headers: {
                    "Content-Type": "application/json"
                },
                auth: {
                    username: process.env.NEO4J_USER,
                    password: process.env.NEO4J_PASS
                }
            });
        } catch (e) {
            console.error("Error on axios request: ", e);
        }

    }
    console.timeEnd('codezup');
    console.log("done");
}

main();

// axios.post(process.env.NEO4J_HOST, req, {
//     headers: {
//         "Content-Type": "application/json"
//     },
//     auth: {
//         username: process.env.NEO4J_USER,
//         password: process.env.NEO4J_PASS
//     }
// }).then(res => {
//     console.timeEnd('codezup');
//     processResult(res.data.results[0].data.map(elt => elt.row));
// });
