from Utils import *

# Part 1

inp = rd("6.txt").strip()

inp = [int(x) for x in inp.split()]

inp2 = [0, 2, 7, 0]


def redist(blocks):
    i = 0
    seen = set()
    seen.add(tuple(blocks))
    dup = dict()

    while True:
        chosen = [x for x, y in enumerate(blocks) if y == max(blocks)][0]
        amt = blocks[chosen]

        dist = 0
        blocks[chosen] = 0
        while dist < amt:
            chosen = (chosen + 1) % len(blocks)
            blocks[chosen] += 1
            dist += 1

        state = tuple(blocks)

        i += 1
        if state in dup:
            return i - dup[state]

        if state in seen:
            dup[state] = i

        seen.add(state)

print(redist(inp))

# 7:40 (> 100th)

# Part 2

# 8:56 (79th)