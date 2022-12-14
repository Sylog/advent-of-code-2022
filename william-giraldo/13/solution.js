function run(left, right) {
    for (let i = 0; i < left.length; i++) {
        if (!right[i] && right[i] !== 0) {
            return false;
        }
        if (typeof left[i] === 'number' && typeof right[i] === 'number') {
            if (left[i] < right[i]) {
                return true;
            }
            if (left[i] > right[i]) {
                return false;
            }
            continue;
        }

        let res;
        if (typeof left[i] === 'number' || typeof right[i] === 'number') {
            res = typeof left[i] === 'number' ? run([left[i]], right[i])  : run(left[i], [right[i]]);
            if (typeof res === 'boolean') {
                return res;
            }
            continue;
        }

        res = run(left[i], right[i]);
        if (typeof res === 'boolean') {
            return res;
        }
    }
    return left.length === right.length ? undefined : true;
    
}
const part = 1;
(function (input) {
    const inputAsArray = input.split('\n\n');
    const D1 = [[2]];
    const D2 = [[6]];
    return part === 1 ? inputAsArray.reduce((acc, curr, i) => {
        const [left, right] = curr.split('\n');
        return acc + (run(JSON.parse(left), JSON.parse(right)) ? (i + 1) : 0);
    }, 0) 
    : inputAsArray
        .map((b) => b.split('\n'))
        .flat()
        .filter((a) => !!a)
        .map((v) => JSON.parse(v))
        .concat([D1, D2])
        .sort((a, b) => {
            const res = run(a, b)
            return res ? -1 : res === false ? 1 : 0;
        })
        .reduce((acc, curr, i) => [D1, D2].includes(curr) ? acc*(i + 1) : acc, 1)
})(
document.body.innerText
);
