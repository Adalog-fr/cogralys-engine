const { readFileSync, writeFileSync } = require("fs");
const { join } = require("path");

const directCall = JSON.parse(readFileSync(
    join(__dirname, "results", "AdaControl_statsByCompilationUnitsWithDirectCallAsisOnly.json"),
    { encoding: "utf8", flag: "r" }
));
const allCall = JSON.parse(
    readFileSync(join(__dirname, "results", "AdaControl_statsByCompilationUnitsWithAsisOnly.json"), {
        encoding: "utf8",
        flag: "r",
    })
);


for (compilationUnit in allCall) {
    if (!(compilationUnit in directCall)) {
        continue;
    }

    for (asisCall in allCall[compilationUnit]) {
        const callSum = allCall[compilationUnit][asisCall];
        allCall[compilationUnit][asisCall] = {
            all: callSum,
            indirect: 0,
            direct: 0
        };
        if (asisCall in directCall[compilationUnit]) {
            allCall[compilationUnit][asisCall].direct = directCall[compilationUnit][asisCall];
        }
        allCall[compilationUnit][asisCall].indirect =
            allCall[compilationUnit][asisCall].all - allCall[compilationUnit][asisCall].direct;
    }
}

writeFileSync(join(__dirname, "results", "AdaControl_statsByCompilationUnitsWithDirectAndIndirectAsisCallOnly.json"), JSON.stringify(allCall, null, 2));
