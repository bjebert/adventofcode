from Read import *
import re
import math

# Part 1

inp = rdl("16.txt")

aunts = dict()

for line in inp:
    ss = line.split(' ')

    aunts[int(ss[1][:-1])] = {ss[2][:-1]: int(ss[3][:-1]),
                              ss[4][:-1]: int(ss[5][:-1]),
                              ss[6][:-1]: int(ss[7])}

mfcsam = """children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1""".split('\n')

aunt_keys = [x.split(': ')[0] for x in mfcsam]
aunt_vals = [int(x.split(': ')[1]) for x in mfcsam]

actual = dict(zip(aunt_keys, aunt_vals))

def is_match1(actual, aunt):
    for k in aunt:
        if actual[k] != aunt[k]:
            return False
    return True


for a in aunts:
    if is_match1(actual, aunts[a]):
        print(a)

# 5:43 (15th)

# Part 2

def is_match2(actual, aunt):
    for k in aunt:
        if k in ['cats', 'trees']:
            if aunt[k] <= actual[k]:
                return False
        elif k in ['pomeranians', 'goldfish']:
            if aunt[k] >= actual[k]:
                return False
        elif actual[k] != aunt[k]:
            return False
    return True


for a in aunts:
    if is_match2(actual, aunts[a]):
        print(a)

# 6:52 (4th)
