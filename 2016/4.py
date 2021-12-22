from Read import *
from Utils import *
import re
import math
import itertools
import functools
from collections import Counter

# Part 1

inp = rdl("4.txt")

# inp = ['aaaaa-bbb-z-y-x-123[abxyz]']

def is_real(code, letters):
    cnt = sort_dict(Counter(code), asc=False)
    new_code = ''

    for v in unique(cnt.values()):
        arr = [x for x in cnt if cnt[x] == v]
        arr.sort()

        i = 0
        while len(new_code) <= 4 and i < len(arr):
            new_code += arr[i]
            i += 1

    return new_code == letters

count = 0
for line in inp:
    code = "".join(line.split('-')[:-1])
    end = line.split('-')[-1].split('[')
    sector = int(end[0])
    letters = end[1].split(']')[0]

    if is_real(code, letters):
        count += sector

print(count)

# 16:54 (95th)

# Part 2

inp = rdl("4.txt")
# inp = ['qzmt-zixmtkozy-ivhz-343']


def decrypt(code, sector):
    sector %= 26

    new_code = ''
    for c in code:
        if c == '-':
            res = ' '
        else:
            x = ord(c)
            for i in range(sector):
                x += 1
                if x == 123:
                    x = 97

            res = chr(x)
        new_code += res

    return new_code


for line in inp:
    code = "-".join(line.split('-')[:-1])
    end = line.split('-')[-1].split('[')
    sector = int(end[0])

    if 'north' in decrypt(code, sector):
        print(decrypt(code, sector), sector)

# 23:26 (79th)