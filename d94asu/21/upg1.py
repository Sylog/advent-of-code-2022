import sys

f = open(sys.argv[1], "r")
ls = f.readlines()
f.close()
tasks = []
store = {}
for l in ls:
	a = l.split(': ')
	b = a[1].split()
	match len(b):
		case 1:
			store[a[0]] = int(b[0])
		case 3:
			tasks.append((a[0], b))

while len(tasks):
	next = []
	for task in tasks:
		v, (a1, op, a2) = task
		if (a1 in store) and (a2 in store):
			match op:
				case '+':
					store[v] = store[a1] + store[a2]
				case '-':
					store[v] = store[a1] - store[a2]
				case '*':
					store[v] = store[a1] * store[a2]
				case '/':
					store[v] = store[a1] / store[a2]
				case _:
					raise RuntimeError("no such operator")
		else:
			next.append(task)
	tasks = next

print(int(store["root"]))
