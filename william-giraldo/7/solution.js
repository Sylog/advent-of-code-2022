const dirSizes = [];
function traverseLessOrEqual(dir, maxSize) {
    // Second part. This is clearly traversing directories more than once,
    // but ¯\_(^-^)_/¯.
    const currDirSize = dirSize(dir);
    if (currDirSize >= maxSize) {
        dirSizes.push(currDirSize);
        Object.entries(dir.children).forEach(([name, entry]) => {
            if (typeof entry === 'object') {
                traverseLessOrEqual(entry, maxSize);
            }
        })
    }
}

// A slight modification checking on `size` wraps it up for the first part.
function dirSize(dir) {
    let size = 0;
    Object.entries(dir.children).forEach(([name, entry]) => {
        if (typeof entry === 'object') {
            size += dirSize(entry);
        } else {
            size += entry;
        }
    });
    return size;
}

const input = document.body.innerText;

const inputAsArray = input.split('\n');
const root = { parent: null, children: {} };
let curr = root;
inputAsArray.forEach((l) => {
    if (!l) return;
    const [sizeOr$, ...commandOrName] = l.split(' ');
    if (sizeOr$ !== "$") {
        curr.children[commandOrName[0]] = sizeOr$ === "dir" ? { parent: curr, children: {} } : +sizeOr$;
    } else {
        const [command, dir] = commandOrName;
        if (command === "cd") {
            if (dir === "..") {
                curr = curr.parent;
            } else if (dir === "/") {
                curr = root;
            } else {
                curr = curr.children[dir];
            }
        }
    }
})

const allSpace = 70000000;
const minSpace = 30000000;
const usedSpace = dirSize(root);
traverseLessOrEqual(root, usedSpace - (allSpace - minSpace));
dirSizes.sort((a, b) => a - b)[0]
