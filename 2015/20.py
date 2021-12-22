from Read import *
import re
import math
import itertools
from functools import reduce

# Part 1


def factors(n):
    return set(reduce(list.__add__,
                ([i, n//i] for i in range(1, int(n**0.5) + 1) if n % i == 0)))


def cap_factors(n):
    return [x for x in factors(n) if n // x <= 50]


inp = 29000000

# house = 1
# while True:
#     pres = sum([x * 10 for x in factors(house)])
#
#     if pres > inp:
#         break
#     house += 1
#
# print(house)

# 4:16 (1st)

# Part 2

house = 665280
while True:
    pres = sum([x * 11 for x in cap_factors(house)])

    if pres > inp:
        break
    house += 1

print(house)

# 6:17 (2nd)
