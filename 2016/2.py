from Read import *
import re
import math
import itertools
import functools

# Part 1

inp = rdl("2.txt")

code = []
pos = 5
for line in inp:
    for c in line:
        if c == "L":
            if pos == 3:
                pos = 2
            elif pos == 4:
                pos = 3
            elif pos == 6:
                pos = 5
            elif pos == 7:
                pos = 6
            elif pos == 8:
                pos = 7
            elif pos == 9:
                pos = 8
            elif pos == "B":
                pos = "A"
            elif pos == "C":
                pos = "B"
        elif c == "R":
            if pos == 2:
                pos = 3
            elif pos == 3:
                pos = 4
            elif pos == 5:
                pos = 6
            elif pos == 6:
                pos = 7
            elif pos == 7:
                pos = 8
            elif pos == 8:
                pos = 9
            elif pos == "A":
                pos = "B"
            elif pos == "B":
                pos = "C"
        elif c == "U":
            if pos == 3:
                pos = 1
            elif pos == 6:
                pos = 2
            elif pos == 7:
                pos = 3
            elif pos == 8:
                pos = 4
            elif pos == "A":
                pos = 6
            elif pos == "B":
                pos = 7
            elif pos == "C":
                pos = 8
            elif pos == "D":
                pos = "B"
        elif c == "D":
            if pos == 1:
                pos = 3
            elif pos == 2:
                pos = 6
            elif pos == 3:
                pos = 7
            elif pos == 4:
                pos = 8
            elif pos == 6:
                pos = "A"
            elif pos == 7:
                pos = "B"
            elif pos == 8:
                pos = "C"
            elif pos == "B":
                pos = "D"
    print(pos)

# 2:51 (3rd)

# Part 2

# 5:00 (1st)
