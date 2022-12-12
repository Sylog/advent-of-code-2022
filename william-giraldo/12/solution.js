// Oh, God, sorry for what you're about to read.
// It is a Dijkstra algorithm implementation, with tweaks for:
//  - Consider adjacent vertical or horizontal cells as nodes.
//  - Consider elevation.

function minUnvisited(M, visited) {
    let min;
    for (let i = 0; i < M.length; i++) {
        for (let j = 0; j < M[0].length; j++) {
            if (!visited[i][j] && (!min || M[i][j] < M[min[0]][min[1]])) {
                min = [i, j];
            }
        }
    }
    return min;
}
function diff(n, p) {
    return n.charCodeAt(0) - p.charCodeAt(0);
}

function matrixDijkstra(start, endChar, M, canJump) {
    const distances = Array(M.length)
        .fill()
        .map(() => Array(M[0].length).fill(Infinity));
    distances[start[0]][start[1]] = 0;
    const visited = distances.map(a => Array(a.length).fill(0));
    let minDistance = Infinity;
    let min;
    while (min = minUnvisited(distances, visited)) {
        visited[min[0]][min[1]] = 1;
        if (M[min[0]][min[1]] === endChar) {
            minDistance = Math.min(minDistance, distances[min[0]][min[1]]);
        }
        if (min[0] + 1 < distances.length &&
            !visited[min[0] + 1][min[1]] &&
            canJump(M[min[0] + 1][min[1]], M[min[0]][min[1]]) <= 1) {
            
            distances[min[0] + 1][min[1]] = Math.min(distances[min[0] + 1][min[1]], distances[min[0]][min[1]] + 1);
        }

        if (min[0] - 1 >=0 &&
            !visited[min[0] - 1][min[1]] &&
            canJump(M[min[0] - 1][min[1]], M[min[0]][min[1]]) <= 1) {
            
            distances[min[0] - 1][min[1]] = Math.min(distances[min[0] - 1][min[1]], distances[min[0]][min[1]] + 1);
        }

        if (min[1] + 1 < distances[0].length  &&
            !visited[min[0]][min[1] + 1] &&
            canJump(M[min[0]][min[1] + 1], M[min[0]][min[1]]) <= 1) {
                distances[min[0]][min[1] + 1] = Math.min(distances[min[0]][min[1] + 1], distances[min[0]][min[1]] + 1);
        }

        if (min[1] - 1 >= 0  &&
            !visited[min[0]][min[1] - 1] &&
            canJump(M[min[0]][min[1] - 1], M[min[0]][min[1]]) <= 1) {
                distances[min[0]][min[1] - 1] = Math.min(distances[min[0]][min[1] - 1], distances[min[0]][min[1]] + 1);
        }
    }

    return minDistance;
}
const part = 1; // 1|2
(function (input) {
    const inputAsArray = input.split('\n');
    const M = [];
    const start = [], end = [];
    inputAsArray.forEach((line, i) => {
        if (line) {
            const S = line.indexOf('S');
            const E = line.indexOf('E');
            if (S >= 0) start.push(i, S);
            if (E >= 0) end.push(i, E);
            M.push(line);
        }
    });
    const fn1 = (n, p) => {    
        if (p === 'S') return 0;
        if (n === 'E') return p === 'z' ? 0 : Infinity;
        return diff(n, p);
    };
    const fn2 = (ch) => (n, p) => {
        if (n === ch) return ['a', 'b'].includes(p) ? 0 : Infinity;
        if (p === 'E') return n === 'z' ? 0 : Infinity;
        return diff(p, n);
    }
    return part === 1 ? matrixDijkstra(start, 'E', M, fn1) : Math.min(matrixDijkstra(end, 'S', M, fn2('S')), matrixDijkstra(end, 'a', M, fn2('a')));;
})(
document.body.innerText
);
