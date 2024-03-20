const { basename, join } = require("path");
const Table = require("tty-table");
const { readFileSync, writeFileSync } = require("fs");

const data = JSON.parse(readFileSync(join(__dirname, "result.json"), { encoding: "utf8", flag: "r" }));

// console.log("data: ", data);

const compilationUnits = {};

const getNodeFullName = (node) => {
    return `${node.content} (${node.enclosing_unit})`;
};

for (const record of data) {
    const caller = record.row[0];
    const subProgsCalled = record.row[1];

    if (!(caller.enclosing_unit in compilationUnits)) {
        compilationUnits[caller.enclosing_unit] = {};
    }

    for (const subProgCalled of subProgsCalled) {
        if (!(subProgCalled.node.enclosing_unit in compilationUnits[caller.enclosing_unit])) {
            compilationUnits[caller.enclosing_unit][getNodeFullName(subProgCalled.node)] = subProgCalled.nbCall;
        }
        compilationUnits[caller.enclosing_unit][getNodeFullName(subProgCalled.node)] += subProgCalled.nbCall;
    }
}

// console.log(compilationUnits);

writeFileSync(join(__dirname, "results", "statsByCompilationUnits.json"), JSON.stringify(compilationUnits, null, 2));

// Filter by ASIS

const regex = /\(Asis/gm;

for (const compilationUnit in compilationUnits) {
    for (const subProgCalled in compilationUnits[compilationUnit]) {
        if (!regex.test(subProgCalled)) {
            delete compilationUnits[compilationUnit][subProgCalled];
        }
    }
}

writeFileSync(join(__dirname, "results", "statsByCompilationUnitsWithAsisOnly.json"), JSON.stringify(compilationUnits, null, 2));
