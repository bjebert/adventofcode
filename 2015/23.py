from Read import *
import re
import math
import itertools
import functools

# Part 1

inp = rdl("23.txt")

a = 1
b = 0

i = 0
while i < len(inp):
    line = inp[i]
    ss = line.split(' ')
    instr = ss[0]

    if instr == 'hlf':
        if ss[1] == 'a':
            a /= 2
        elif ss[1] == 'b':
            b /= 2
        i += 1
    elif instr == 'tpl':
        if ss[1] == 'a':
            a *= 3
        elif ss[1] == 'b':
            b *= 3
        i += 1
    elif instr == 'inc':
        if ss[1] == 'a':
            a += 1
        elif ss[1] == 'b':
            b += 1
        i += 1
    elif instr == 'jmp':
        amt = int(ss[1])
        i += amt
    elif instr == 'jie':
        reg = ss[1][0]
        amt = int(ss[2])

        if reg == 'a' and a % 2 == 0:
            i += amt
        elif reg == 'b' and b % 2 == 0:
            i += amt
        else:
            i += 1
    elif instr == 'jio':
        reg = ss[1][0]
        amt = int(ss[2])

        if reg == 'a' and a == 1:
            i += amt
        elif reg == 'b' and b == 1:
            i += amt
        else:
            i += 1

print(b)

# 6:24 (3rd)

# Part 2

# 6:37 (3rd)