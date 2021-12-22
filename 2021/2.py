from Utils import *

# Part 1

inp = rdl("2.txt")

pos = [0, 0]

for line in inp:
    ls = line.split(' ')

    if ls[0] == 'down':
        pos[0] += int(ls[1])
    elif ls[0] == 'forward':
        pos[1] += int(ls[1])
    elif ls[0] == 'up':
        pos[0] -= int(ls[1])

print(pos[1] * pos[0])

# 1:05 (11th)

# Part 2

pos = [0, 0]
aim = 0

for line in inp:
    ls = line.split(' ')

    if ls[0] == 'down':
        aim += int(ls[1])
    elif ls[0] == 'forward':
        pos[1] += int(ls[1])
        pos[0] += int(ls[1]) * aim
    elif ls[0] == 'up':
        aim -= int(ls[1])

print(pos[1] * pos[0])

# 1:53 (9th)
