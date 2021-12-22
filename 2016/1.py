from Read import *
import re
import math
import itertools
import functools

# Part 1

inp = rd("1.txt")
instr = inp.split(', ')

# instr = ['R8', 'R4', 'R4', 'R8']

facing = "N"
loc = [0, 0]

visited = set()

for inst in instr:
    direc = inst[0]
    amt = int(inst[1:])

    if direc == "L":
        if facing == "N":
            facing = "W"
        elif facing == "W":
            facing = "S"
        elif facing == "S":
            facing = "E"
        elif facing == "E":
            facing = "N"
    elif direc == "R":
        if facing == "N":
            facing = "E"
        elif facing == "W":
            facing = "N"
        elif facing == "S":
            facing = "W"
        elif facing == "E":
            facing = "S"

    if facing == "N":
        for i in range(1, amt+1):
            loc[1] += 1
            if tuple(loc) in visited:
                print(loc)
            visited.add(tuple(loc))

    elif facing == "S":
        for i in range(1, amt + 1):
            loc[1] -= 1
            if tuple(loc) in visited:
                print(loc)
            visited.add(tuple(loc))
    elif facing == "E":
        for i in range(1, amt + 1):
            loc[0] += 1
            if tuple(loc) in visited:
                print(loc)
            visited.add(tuple(loc))
    elif facing == "W":
        for i in range(1, amt + 1):
            loc[0] -= 1
            if tuple(loc) in visited:
                print(loc)
            visited.add(tuple(loc))

# 2:36 (1st)

# Part 2

# 5:59 (1st)

