const data = require("../examples/adactl/wks/req.json");

function createParams(params) {
    return ":params " + JSON.stringify(params);
}

const stmtIdentifier = data[2].statements[124];
console.log(createParams(stmtIdentifier.parameters));
console.log("");
console.log(stmtIdentifier.statement);

const stmtIntLit = data[2].statements[125];
console.log(createParams(stmtIntLit.parameters));
console.log("");
console.log(stmtIntLit.statement);
