# observe: 99x = 1 + 10x + 10^(ceil(lg(x))+1)
# thus: 89 = (1 + 10^i) / x
# and: x = (1 + 10^i) / 89 for some i
# iterate over i and check...
# if divisable by 89 check condition

i = 1
while True:
    if (10 ** i + 1) % 89 == 0:
        sol = (10 ** i + 1) // 89
        if str(sol * 99) == f'1{sol}1':
            print("First solution:", sol)
            print(99*sol)
            print(f'1{sol}1')
            break
    i+=1

#First solution: 112359550561797752809
#11123595505617977528091
#11123595505617977528091
