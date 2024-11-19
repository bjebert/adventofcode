from Read import *
import re
import math
import itertools
from collections import defaultdict

# Part 1

inp = rdl("../../inputs/2015-19.txt")

target = inp[-1]


def create_map(reverse_map=False):
    sub_map = dict()
    ki = 1 if reverse_map else 0
    vi = 1 - ki

    for line in inp[:-3]:
        k = line.split(' => ')[ki]
        v = line.split(' => ')[vi]

        if k not in sub_map:
            sub_map[k] = [v]
        else:
            sub_map[k].append(v)

    return sub_map


def get_transformations(text, sub_map):
    matches = [list(re.finditer(k, text)) for k in sub_map]
    matches = [m for m in matches if len(m)]

    if len(matches) == 0:
        return []

    transformations = []

    for match in matches:
        sub_strings = [sub_map[m.group()] for m in match]
        transformations += [text[:match[i].span()[0]] + x + text[match[i].span()[1]:] for i in range(len(match)) for x in sub_strings[i]]

    return list(set(transformations))



Q = [(target, 0)]
v = defaultdict(int)
v[target] = 0

min_steps = math.inf
sub_map = create_map(reverse_map=True)
i = 0

while len(Q) > 0:
    i += 1
    text, steps = Q.pop(0)
    # print(f"Step {i}: Length: {len(text)}, Current text: {text}")

    if text == 'e':
        if steps < min_steps:
            min_steps = steps
            print(f'*******{min_steps}********')
        continue

    transformations = get_transformations(text, sub_map)
    transformations = sorted(transformations, key=lambda x: (-len(x), x))

    # Sort transformations by descending length, and alphabetically when lengths are equal

    valid_tf = [[t, steps+1] for t in transformations if v[t] == 0 or steps < v[t]]
    # valid_tf = sorted(valid_tf, key=lambda x: -len(x[0]))

    # print(len(Q))
    # print(len(transformations[0]))
    # if i == 4:
    #     print(text)
    #     print(len(transformations))
    #     # print(len(valid_tf))
    #     break

    # Add these transformations to the queue (sorted by length)
    for t, s in valid_tf:
        # Early break after first valid transformation for pruning (DFS-like behavior)
        Q.insert(0, (t, s))
        v[t] = s
        # break  # Early break similar to the DFS solution: prioritize the first valid one

    # if i % 1000 == 0:
    #     print(len(Q))

