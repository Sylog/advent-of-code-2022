import sys

f = open(sys.argv[1], "r")
ls = f.readlines()
f.close()
tasks = []
store = {}
root1 = ""
root2 = ""

def inverse(op):
	match op:
		case '+':
			return '-'
		case '-':
			return '+'
		case '*':
			return '/'
		case '/':
			return '*'
		case _:
			raise "no such operator"

def commutative(op):
	match op:
		case '+':
			return True
		case '*':
			return True
		case _:
			return False

def calc(op, v, a1, a2):
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
			raise "no such operator"

def tryOperate(task):
	v, (a1, op, a2) = task
	hasv = v in store
	hasa1 = a1 in store
	hasa2 = a2 in store
	if hasa1 and hasa2:
		calc(op, v, a1, a2)
		return True
	elif hasv and hasa2:
		calc(inverse(op), a1, v, a2)
		return True
	elif hasv and hasa1:
		if commutative(op):
			calc(inverse(op), a2, v, a1)
		else:
			calc(op, a2, a1, v)
		return True
	else:
		return False
		
	
for l in ls:
	a = l.split(': ')
	b = a[1].split()
	match len(b):
		case 1:
			if a[0] != "humn":
				store[a[0]] = int(b[0])
		case 3:
			if a[0] == "root":
				root1 = b[0]
				root2 = b[2]
			else:
				tasks.append((a[0], b))

nlast = 0
n = len(tasks)
while nlast != n:
	nlast = n
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
	n = len(tasks)

if root1 in store:
		    store[root2] = store[root1]
		    nlast = 0
elif root2 in store:
		    store[root1] = store[root2]
		    nlast = 0
else:
	raise RuntimeError("did not reach root")

nlast = 0
n = len(tasks)
while n != nlast:
	nlast = n
	next = []
	for task in tasks:
		if not tryOperate(task):
			next.append(task)
	tasks = next
	n = len(tasks)

print(int(store["humn"]))
