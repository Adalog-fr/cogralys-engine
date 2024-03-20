const { readFileSync, writeFileSync } = require('fs');

const str = readFileSync("./adactl_input.txt", { encoding: "utf8", flag: "r" });

// 10 elt
const regex10 = /^([\w.-]+):0*(\d+):(\d+): Found: USAGE: \((\w+)\) ([^,]+), ([^,]+), ([^,]+), ([^,]+), ([^,]+), ([^\n]+)$/g;
const subst10 = `{ "filename": "$1", "line": $2, "column": $3, "origin": "$4", "content": "$5", "visibility": "$6", "is_initialized": "$8", "is_written": "$9", "is_read": "$10" },`;

// elt
const regex9 = /^([\w.-]+):0*(\d+):(\d+): Found: USAGE: \((\w+)\) ([^,]+), ([^,]+), ([^,]+), ([^,]+), ([^\n]+)$/g;
const subst9 = `{ "filename": "$1", "line": $2, "column": $3, "origin": "$4", "content": "$5", "visibility": "", "is_initialized": "$7", "is_written": "$8", "is_read": "$9" },`;

const regex7 = /^([\w.-]+):0*(\d+):(\d+): Found: USAGE: \((\w+)\) ([^,]+), ([^,]+), ([^\n]+)$/g;
const subst7 = `{ "filename": "$1", "line": $2, "column": $3, "origin": "$4", "content": "$5", "visibility": "", "is_initialized": "", "is_written": "not written", "is_read": "$7" },`;

let acc= "";

for (const line of str.split("\n")) {
    if (line === "") {
        continue;
    }

    if (regex10.test(line)) {
        acc += line.replace(regex10, subst10);
    } else if (regex9.test(line)) {
        acc += line.replace(regex9, subst9);
    } else if (regex7.test(line)) {
        acc += line.replace(regex7, subst7);
    } else {
        console.error("cannot match line: ", line);
    }
}

const result = JSON.parse(`[${acc.slice(0, -1)}]`).sort((a, b) => {
    if (a.filename > b.filename) {
        return -1;
    }
    if (b.filename > a.filename) {
        return 1;
    }

    if (parseInt(a.line) > parseInt(b.line)) {
        return -1;
    }

    if (parseInt(a.line) < parseInt(b.line)) {
        return 1;
    }

    if (parseInt(a.column) > parseInt(b.column)) {
        return -1;
    }

    if (parseInt(a.column) < parseInt(b.column)) {
        return 1;
    }

    return 0;
}).reverse();

for (const elt of result) {
    elt.is_written = elt.is_written === "written";
    const regexNote = /^(?:not )?read \(([\w\s]+)\)$/g;
    elt.note = regexNote.test(elt.is_read) ? elt.is_read.replace(regexNote, "$1") : "";
    elt.is_read = /^read/g.test(elt.is_read);
}

writeFileSync("adactl_result.json", JSON.stringify(result, null, 2), { encoding: "utf8", flag: "w" });
