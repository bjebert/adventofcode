from Read import *
import re
import math
import itertools
import functools

# Part 1

inp = rdl("3.txt")
inp = [x.strip() for x in inp]

z = [line.split(' ') for line in inp]
ints = []
count = 0
for i in z:
    arr = [int(x) for x in i if x != '']
    ints.append(tuple(arr))
    arr.sort()

    if arr[0] + arr[1] > arr[2]:
        count += 1

print(count)

# 6:12 (> 100th)

t1 = [x[0] for x in ints]
t2 = [x[1] for x in ints]
t3 = [x[2] for x in ints]

count = 0
for x in [t1, t2, t3]:
    for i in range(0, len(x), 3):
        arr = [x[i], x[i+1], x[i+2]]
        arr.sort()

        if arr[0] + arr[1] > arr[2]:
            count += 1

print(count)

# 10:07 (74th)
