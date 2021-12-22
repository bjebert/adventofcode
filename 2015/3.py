# Part 1

with open('3.txt', 'r') as f:
    inp = f.read()

    deliveries = {(0, 0)}
    curr_pos = [0, 0]

    for c in inp:
        if c == '>':
            curr_pos[0] += 1
        elif c == '<':
            curr_pos[0] -= 1
        elif c == '^':
            curr_pos[1] += 1
        elif c == 'v':
            curr_pos[1] -= 1

        deliveries.add(tuple(curr_pos))

    print(len(deliveries))

# Part 2

with open('3.txt', 'r') as f:
    inp = f.read()

    deliveries = {(0, 0)}

    cp = ([0, 0], [0, 0])

    for i in range(len(inp)):
        mover = 0 if i % 2 == 0 else 1

        c = inp[i]
        if c == '>':
            cp[mover][0] += 1
        elif c == '<':
            cp[mover][0] -= 1
        elif c == '^':
            cp[mover][1] += 1
        elif c == 'v':
            cp[mover][1] -= 1

        deliveries.add(tuple(cp[mover]))

    print(len(deliveries))









