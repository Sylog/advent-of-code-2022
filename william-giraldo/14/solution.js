// Part 2 solution. Part 1 requires some small removals from L55-L58.
(function (input) {
    const inputAsArray = input.split('\n');
    let minX = Infinity, maxX = 500, maxY = 0;
    inputAsArray.forEach((line) => {
        if (!line) return;
        const pairs = line.split(' -> ');
        pairs.forEach((pair) => {
            const [a, b] = pair.split(',').map(x => +x);
            minX = Math.min(a, minX);
            maxX = Math.max(a, maxX);
            maxY = Math.max(b, maxY);
        });
    });
    maxY += 2;
    const M = Array(maxY + 1).fill().map(() => Array(maxX+1).fill('.'));

    inputAsArray.forEach((line) => {
        if (!line) return;
        const pairs = line.split(' -> ');
        let ptr;
        pairs.forEach((pair) => {
            const [x, y] = pair.split(',').map(a => +a);
            if (ptr) {
                const isRow = ptr[1] === y;
                const begin = isRow ? Math.min(ptr[0], x) : Math.min(ptr[1], y);
                const end = isRow ? Math.max(ptr[0], x) : Math.max(ptr[1], y);
                for (
                    let i = begin;
                    i <= end;
                    i++
                    ) {
                        M[isRow ? y : i][isRow ? i : x] = "#";
                    }
            }
            ptr = [x, y];
        });
    });
    
    M[maxY] = Array(maxX+1).fill("#");

    let stop = false;
    let initialPos = [500, 0], pos = initialPos;
    let cnt = 0;
    while (!stop) {
        const [x, y] = pos;
        if (y < maxY - 1) {
            if (M[y + 1][x] === ".") {
                pos = [x, y + 1];
            } else if (M[y + 1][x - 1] === ".") {
                pos = [x - 1, y + 1];
            } else if (M[y + 1][x + 1] === ".") {
                pos = [x + 1, y + 1];
            } else {
                if (x - 1 < 0) {
                    M.forEach((r) => r.unshift('.'));
                    initialPos[0] += 1;
                    continue;
                }
                if (x + 1 > maxX) {
                    M.forEach((r) => r.push('.'));
                    maxX++;
                    continue;
                }

                if (pos[0] === initialPos[0] && pos[1] === initialPos[1]) {
                    stop = true;
                } else {
                    M[y][x] = 'o';
                    pos = initialPos;
                }
                cnt++;
            }
        } else {
            pos = initialPos;
            M[y][x] = 'o';
            cnt++;
        }
        
    }
    return cnt + 1;
})(
document.body.innerText
);
