// Ran in the browser, hence the document innerText API usage
const input = document.body.innerText;

let i = 0;
let k = 4; // Set k to 4 or 14 for the first/second part, respectively
for (i=k - 1; i < input.length && (new Set(input.slice(i - (k - 1), i + 1))).size !== k; i++);
console.log(i + 1);
