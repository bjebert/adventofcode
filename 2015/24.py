from Read import *
import re
import math
import itertools
import functools
from functools import reduce


# Part 1

inp = [int(x) for x in rdl("24.txt")]

# inp = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11]

group_wt = int(sum(inp) / 4)


for amt in range(1, 8):
    print(amt)
    min_qe = math.inf
    found = False
    for x in itertools.permutations(inp, amt):
        if sum(x) == group_wt:
            found = True
            qe = reduce(lambda x, y: x * y, x, 1)
            if qe < min_qe:
                min_qe = qe
                print(qe)

    if found:
        break

# 7:55 (5th)

# Part 2

# 9:23 (5th)