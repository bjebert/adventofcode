from Read import *
import re
import math
from itertools import combinations, permutations

# Part 1

inp = rdl("13.txt")

# inp = """Alice would gain 54 happiness units by sitting next to Bob.
# Alice would lose 79 happiness units by sitting next to Carol.
# Alice would lose 2 happiness units by sitting next to David.
# Bob would gain 83 happiness units by sitting next to Alice.
# Bob would lose 7 happiness units by sitting next to Carol.
# Bob would lose 63 happiness units by sitting next to David.
# Carol would lose 62 happiness units by sitting next to Alice.
# Carol would gain 60 happiness units by sitting next to Bob.
# Carol would gain 55 happiness units by sitting next to David.
# David would gain 46 happiness units by sitting next to Alice.
# David would lose 7 happiness units by sitting next to Bob.
# David would gain 41 happiness units by sitting next to Carol.""".split('\n')

happiness = dict()

for line in inp:
    person = line.split(' ')[0]
    adversary = line.split(' ')[-1][:-1]

    is_gain = line.split(' ')[2] == 'gain'
    hap = int(line.split(' ')[3])

    if not is_gain:
        hap *= -1

    if person not in happiness:
        happiness[person] = dict()
    happiness[person][adversary] = hap

perms = list(permutations(list(happiness.keys()), len(happiness)))

max_hap = -99999
for p in perms:
    curr_hap = 0

    for k in range(len(p)):
        if k == 0:
            curr_hap += happiness[p[k]][p[len(p)-1]]
        else:
            curr_hap += happiness[p[k]][p[k-1]]

        if k == len(p) - 1:
            curr_hap += happiness[p[k]][p[0]]
        else:
            curr_hap += happiness[p[k]][p[k+1]]

    if curr_hap > max_hap:
        max_hap = curr_hap

print(max_hap)

# 10:18 (19th)

# Part 2

happiness['You'] = dict()
for p in happiness:
    happiness[p]['You'] = 0
    happiness['You'][p] = 0

# 11:18 (13th)