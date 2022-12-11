function mul_inv(a, b){
    var b0 = b;
    var x0 = 0;
    var x1 = 1;
    var q, tmp;
    if( b== 1){
      return 1;
    }
    while(a>1){
      q = parseInt(a/b);
      tmp = a;
      a = b;
      b = tmp%b;
      tmp = x0;
      x0 = x1 - (q * x0);
      x1 = tmp;
    }
    if(x1 <0){
      x1 = x1+b0;
    }
    return x1;
  }
  
function chineseRemainder(a, n){
    var p = 1;
    var i = 1;
    var prod = 1;
    var sm = 0;
    for(i=0; i< n.length; i++){
        prod = prod * n[i];
    }
    for(i=0; i< n.length; i++){
        p = prod / n[i];
        sm = sm + ( a[i] * mul_inv(p, n[i]) * p);
    }
    return sm % prod;
}
// Got all of the above from https://github.com/pnicorelli/nodejs-chinese-remainder/blob/master/chinese_remainder.js
// You can assume they don't exist for part 1.

const part = 1; // 1|2
(function (input) {
    const inputAsArray = input.split('\n\n');
    const monkeys = Array(inputAsArray.length).fill();
    let MOD = [];
    inputAsArray.forEach((monkey) => {
        const attrs = monkey.split('\n');
        const num = +/Monkey (\d+)/.exec(attrs[0])[1];
        const startingItems = /Starting items: (\d+.*)/.exec(attrs[1])[1].split(', ').map(i => +i);
        const op = /Operation: new = old (.) (\d+|old)/.exec(attrs[2]).slice(1, 3);
        const mod = +/divisible by (\d+)/.exec(attrs[3])[1];
        const dstIfTrue = +/throw to monkey (\d+)/.exec(attrs[4])[1];
        const dstIfFalse = +/throw to monkey (\d+)/.exec(attrs[5])[1];

        monkeys[num] = { startingItems, op, mod, dstIfTrue, dstIfFalse, inspect: 0 };
        MOD.push(mod);
    });
    for (let i = 0; i < (part === 1 ? 20 : 10000); i++) {
        monkeys.forEach((monkey) => {
            const op = monkey.op[0];
            monkey.inspect += monkey.startingItems.length;
            monkey.startingItems.forEach((item) => {
                const value = monkey.op[1] !== 'old' ? +monkey.op[1] : item;
                let worry = item;
                switch (op) {
                    case '+':
                        worry += value;
                        break;
                    case '*':
                        worry *= value;
                        break;
                }
                worry = worry / (part === 1 ? 3 : 1);
                worry = worry >>> 0;
                const dstMonkey = worry % monkey.mod === 0 ? monkey.dstIfTrue : monkey.dstIfFalse;
                monkeys[dstMonkey].startingItems.push(
                    part === 1 ? worry : chineseRemainder(MOD.map(m => worry % m), MOD)
                    // worry
                );
            });
            monkey.startingItems = [];
        });
    }
    monkeys.sort(({ inspect: a }, { inspect: b }) => b - a);
    return monkeys[0].inspect * monkeys[1].inspect;
})(
document.body.innerText
);
