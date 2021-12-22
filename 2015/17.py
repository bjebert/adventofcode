from Read import *
import re
import math
import itertools

# Part 1

inp = rdl("17.txt")

inp = [int(x) for x in inp]

# example
# inp = [20, 15, 10, 5, 5]
amt = 150

count = 0
for i in range(1, len(inp) + 1):
    com = list(itertools.combinations(inp, i))
    count += sum([sum(x) == amt for x in com])

print(count)

# 2:21 (5th)

count = 0
for i in range(1, len(inp) + 1):
    com = list(itertools.combinations(inp, i))
    count += sum([sum(x) == amt for x in com])

    if count > 0:
        break

print(count)

# 2:55 (2nd)