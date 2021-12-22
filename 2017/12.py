from Utils import *

# Part 1

inp = rdl("12.txt")

programs = dict()

for line in inp:
    ls = line.split(' <-> ')
    programs[int(ls[0])] = [int(x) for x in ls[1].split(', ')]


def get_group(start):
    queue = [start]
    group = []
    while len(queue):
        curr = queue.pop()
        group.append(curr)

        for p in programs[curr]:
            if p not in group:
                queue.append(p)

    return group

print(len(get_group(0)))

# 2:47 (T-13th)


q = list(programs.keys())
groups = []

while len(q):
    curr = q.pop(0)
    grp = get_group(curr)

    groups.append(grp)

    for g in grp:
        if g in q:
            q.remove(g)

print(len(groups))

# 5:44 (30th)

