from Read import *
from Utils import *
import re
import math
from itertools import permutations, combinations, product
import functools
from collections import Counter
import hashlib

# Part 1

# inp = 'ffykfhsq'
#
# i = 0
# psw = []
# while True:
#     z = inp + str(i)
#     enc = hashlib.md5(z.encode('utf-8')).hexdigest()
#
#     if enc[:5] == '00000':
#         psw.append(enc[5])
#
#     i += 1
#
#     if len(psw) == 8:
#         break
#
# print("".join(psw))

# 4:09 (11th)

inp = 'ffykfhsq'
# inp = 'abc'

i = 0
psw = [-1, -1, -1, -1, -1, -1, -1, -1]
count = 0
while True:
    i += 1
    z = inp + str(i)
    enc = hashlib.md5(z.encode('utf-8')).hexdigest()

    if enc[:5] == '00000':
        if enc[5].isnumeric():
            pos = int(enc[5])
            v = enc[6]

            if pos >= len(psw) or pos < 0:
                continue

            if psw[pos] != -1:
                continue

            psw[pos] = v
            count += 1
            print(psw)

    if count == 8:
        break

print(psw)

# 12:07 (34th)

