const part = 1; // 1|2
(function (input) {
    const inputAsArray = input.split('\n');
    let pc = 0, X = 1, s = part === 1 ? 0 : "";
    inputAsArray.forEach((step) => {
        if (!step) return;
        const [op, v] = step.split(' ');
        for (let i = 0; i < (op === 'noop' ? 1 : 2); i++) {
            if (part === 2) {
                if (pc % 40 === 0) {
                    s += '\n';
                    pc = 0;
                }
                s += (pc >= X - 1 && pc <= X + 1) ? '#' : '.'; 
            } else {
                s += (pc + 21) % 40 === 0 ? X*(pc + 1) : 0; 
            }
            pc++;
        }
        X += op === 'addx' ? +v : 0;
    });
    return s;
})(
document.body.innerText
);
