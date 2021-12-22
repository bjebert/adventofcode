from Read import *
import re
import math
import itertools
import functools

# Part 1


def get_i(row, col):
    r_i = 1
    inc = 1
    for i in range(row - 1):
        r_i += inc
        inc += 1

    start = r_i
    inc = row+1
    for i in range(col - 1):
        start += inc
        inc += 1

    return start


x = 20151125
for i in range(get_i(2947, 3029) - 1):
    x = (x * 252533) % 33554393

print(x)

# 8:46 (20th) / 8:49 (19th)
