from Read import *
import re
import math
import itertools

# Part 1

inp = rdl("../inputs/2015-19.txt")

mol = inp[-1]
subs = dict()

for line in inp[:-3]:
    k = line.split(' => ')[0]
    v = line.split(' => ')[1]

    if k not in subs:
        subs[k] = [v]
    else:
        subs[k].append(v)

# mols = set()

# subs = {'H': ['HO', 'OH'],
#         'O': ['HH']}
# mol = "HOH"

# for k in subs:
#     for s in subs[k]:
#         for m in re.finditer(k, mol):
#             new_mol = mol[:m.span()[0]] + s + mol[m.span()[1]:]
#             mols.add(new_mol)
#
# print(len(mols))

# 14:04 (41st)

# Part 2
# inp = rdl("19.txt")

# subs = {'e': ['H', 'O'],
#         'H': ['HO', 'OH'],
#         'O': ['HH']}
# mol = "HOH"

# Must start from e

curr = 'e'

# Substitutions never get shorter - so if we exceed length of goal string, then give up

# def build(curr, mol, steps=0, tried=[]):
#     if curr == mol:  # Got it!
#         return steps
#
#     if len(curr) >= len(mol):  # Abort
#         return math.inf
#
#     # Find all possible substitutes
#     matches = [list(re.finditer(k, curr)) for k in subs]
#     matches = [m for m in matches if len(m)]
#     matches = [x for y in matches for x in y]
#
#     min_steps = 99999
#     for m in matches:
#         for k in subs[m[0]]:
#             new_curr = curr[:m.span()[0]] + k + curr[m.span()[1]:]
#
#             if new_curr not in tried:
#                 tried.append(new_curr)
#                 res = build(new_curr, mol, steps + 1, tried)
#
#                 if res < min_steps:
#                     print(res)
#                     min_steps = res
#
#     if len(tried) % 100 == 0:
#         print(len(tried))
#
#     return min_steps
#
#
# print(build(curr, mol))  # This should hypothetically work but takes too long for this use case

# Let's try a greedy, max sub approach first starting from the full mol working in reverse


def destroy(curr, mol, steps=0, tried=[]):
    if curr == 'e':  # Got it!
        return steps
    elif len(curr) <= 1:  # Abort
        return math.inf

    # Find all possible substitutes
    patterns = list(set([x for y in list(subs.values()) for x in y]))

    matches = [list(re.finditer(pat, curr)) for pat in patterns]
    matches = [m for m in matches if len(m)]
    matches = [x for y in matches for x in y]

    min_steps = 99999

    # Sort from biggest reduction to smallest
    matching_keys = set([x[0] for x in matches])
    len_dict = dict(zip(matching_keys, [len(x) for x in matching_keys]))
    sorted_subs = list(dict(sorted(len_dict.items(), key=lambda item: item[1], reverse=True)).keys())

    for ss in sorted_subs:
        for m in [m for m in matches if m[0] == ss]:

            # Which key belongs to this match?
            mk = [k for k in subs if m[0] in subs[k]][0]
            new_curr = curr[:m.span()[0]] + mk + curr[m.span()[1]:]

            if new_curr not in tried:
                tried.append(new_curr)
                res = destroy(new_curr, mol, steps + 1, tried)

                if res < min_steps:
                    print(res)
                    min_steps = res

    return min_steps

print(destroy(mol, mol))

# 58:27 (31st)


