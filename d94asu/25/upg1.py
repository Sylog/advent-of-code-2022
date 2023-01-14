import sys

def toInt(s):
    x = 0
    for c in s:
        match c:
            case '=':
                i = -2
            case '-':
                i = -1
            case '0':
                i = 0
            case '1':
                i = 1
            case '2':
                i = 2
            case _:
                i = -99
        if i == -99:
            return x
        else:
            x = i + 5 * x
    return x

def toChar(i):
	match i:
		case -2: return "="
		case -1: return "-"
		case _: return str(i)

def toString(x):
	l1 = []
	l2 = []
	mem = 0
	while x > 0:
		i = x % 5
		x = x // 5
		l1.append(i)
	for i in l1:
		i = i + mem
		if i > 2:
			l2.insert(0, i - 5)
			mem = 1
		else:
			l2.insert(0, i)
			mem = 0
	l3 = map(toChar, l2)
	return "".join(l3)

f = open(sys.argv[1], "r")
ls = f.readlines()
f.close()
print(toString(sum(list(map(toInt, ls)))))
