from Utils import *

# Part 1

inp = rd("11.txt").split(',')

furth = 0
pos = [0, 0]
for instr in inp:
    if instr == 'n':
        pos[0] += 1
    elif instr == 's':
        pos[0] -= 1
    elif instr == 'nw':
        pos[0] += 0.5
        pos[1] -= 0.5
    elif instr == 'ne':
        pos[0] += 0.5
        pos[1] += 0.5
    elif instr == 'sw':
        pos[0] -= 0.5
        pos[1] -= 0.5
    elif instr == 'se':
        pos[0] -= 0.5
        pos[1] += 0.5

    dist = sum([abs(x) for x in pos])
    if dist > furth:
        print(dist)
        furth = dist

print(sum([abs(x) for x in pos]))

# 7:29 (73rd)

# Part 2

# 8:35 (44th)


