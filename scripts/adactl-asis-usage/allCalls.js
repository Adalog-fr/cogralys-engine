const { join } = require("path");
const { readFileSync } = require("fs");

const allCalls = JSON.parse(readFileSync(join(__dirname, "allCalls.json"), { encoding: "utf8", flag: "r" }));

console.log("allCalls size: ", allCalls.length);

const allCallsObject = {};

for (const element of allCalls) {
    allCallsObject[element.row[0].node_id] = {
        row: { self: element.row[0], selfIdNode: element.row[1], calls: element.row[2] },
    };
}

const compilationUnits = {};

function processNode(startNodeId, currentNodeProcessed, accumulator) {
    const currentObj = allCallsObject[currentNodeProcessed];
    if (!currentObj) {
        throw Error(`Unknown node "${currentNodeProcessed}"`);
    }
    const compilationUnitName = currentObj.row.selfIdNode.enclosing_unit;
    const currentSubProgramName = currentObj.row.selfIdNode.content;
    if (!compilationUnits[compilationUnitName]) {
        compilationUnits[compilationUnitName] = {};
    }
    if (!compilationUnits[compilationUnitName][currentSubProgramName]) {
        compilationUnits[compilationUnitName][currentSubProgramName] = {};
    }
    for (const call of currentObj.row.calls) {
        const currentCallName = `${call.nodeId.content} (${call.nodeId.enclosing_unit})`;
        if (!compilationUnits[compilationUnitName][currentSubProgramName][currentCallName]) {
            compilationUnits[compilationUnitName][currentSubProgramName][currentCallName] = 0;
        }
        const currentAcc = call.nbCall * accumulator;
        compilationUnits[compilationUnitName][currentSubProgramName][currentCallName] += currentAcc;

        if (call.node.node_id in allCallsObject) {
            processNode(startNodeId, call.node.node_id, currentAcc);
        }
    }
}

const testNodeId =
    "/examples/Adacontrol-archi/Adacontrol/src/rules-max_statement_nesting.ads:34:4:A_PROCEDURE_DECLARATION";


// console.log(allCallsObject[testNodeId]);

// for (const key in allCallsObject) {
//     if (key.includes("/workspaces/Adacontrol-archi/Asiscomps/src/utilities.ads")) {
//         console.log(key);
//     }
// }

processNode(testNodeId, testNodeId, 1);

console.log(compilationUnits);
