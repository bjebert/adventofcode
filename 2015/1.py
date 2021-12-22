# Part 1

with open('1.txt', 'r') as f:
    inp = f.read()
    print(inp.count('(') - inp.count(')'))

# Part 2

with open('1.txt', 'r') as f:
    inp = f.read()

    floor = 0
    for i in range(len(inp)):
        if inp[i] == '(':
            floor += 1
        else:
            floor -= 1

        if floor < 0:
            print(i+1)
            break





