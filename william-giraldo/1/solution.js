// Ran in the browser, hence the document innerText API usage
const input = document.body.innerText;

const inputAsArray = input.split('\n');
const k = 1; // The k first elves with most calories
let max = Infinity, s = 0;
for (let i = 0; i < k; i++) {
    let counter = 0, maxSoFar = 0;
    inputAsArray.forEach((c) => {
        if (c === '') {
            counter = 0;
        } else {
            counter += +c;
        }
    
        if (counter > maxSoFar && counter < max) {
            maxSoFar = counter;
        }
    });
    console.log(maxSoFar);
    s += max = maxSoFar;
}