function isTouching([a, b], [c, d]) {
    if (a === c) {
        return Math.abs(b - d) <= 1;
    }

    if (b === d) {
        return Math.abs(a - c) <= 1;
    }

    return Math.abs(b - d) === 1 && Math.abs(a - c) == 1;
}

const DIRECTIONS = {
    U: [-1, 0],
    D: [1, 0],
    L: [0, -1],
    R: [0, 1]
};
const NUM_KNOTS = 10; // 2 for first part, 10 for second part.
(function (input) {
    const inputAsArray = input.split('\n');
    const HEAD = [0, 0];
    const TAILS = Array(NUM_KNOTS).fill().map(_ => [0, 0]);
    TAILS[0] = HEAD;
    const places = new Set(['0,0']);
    inputAsArray.forEach((step) => {
        if (!step) return;
        const [direction, units] = step.split(' ');
        for (let i = 0; i < +units; i++) {
            const D = DIRECTIONS[direction];
            HEAD[0] += D[0];
            HEAD[1] += D[1];
            for (let j = 1; j < NUM_KNOTS; j++) {
                let H = TAILS[j - 1];
                let T = TAILS[j];
                if (!isTouching(H, T)) {
                    if (H[0] === T[0]) {
                        T[1] += H[1] > T[1] ? 1 : -1;
                    } else if (H[1] === T[1]) {
                        T[0] += H[0] > T[0] ? 1 : -1;
                    } else {
                        T[1] += H[1] > T[1] ? 1 : -1;
                        T[0] += H[0] > T[0] ? 1 : -1;
                    }
                }
            }
            places.add(TAILS[NUM_KNOTS - 1].join(','));
        }
    });
    return places.size;
})(
document.body.innerText
);
