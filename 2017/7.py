from Utils import *

# Part 1

inp = rdl("7.txt")

towers = dict()
weights = dict()

for line in inp:
    tower = line.split()[0]
    weights[tower] = int(line.split()[1].replace('(', '').replace(')', ''))
    if len(line.split()) > 2:
        sub_towers = line.split()[3:]
        towers[tower] = [x.replace(',', '') for x in sub_towers]
    else:
        towers[tower] = []

# 2:53 (9th)


def get_weight(tower):
    w = []
    for sub_tower in towers[tower]:
        w.append(get_weight(sub_tower))

    if len(unique(w)) > 1:
        print(tower)

    return sum(w) + weights[tower]


print(get_weight('cqmvs'))

# 11:41 (8th)