from Read import *
import math

inp = rdl("9.txt")

# inp = """London to Dublin = 464
# London to Belfast = 518
# Dublin to Belfast = 141""".split('\n')

# Part 1

travel = dict()

for line in inp:
    sp = line.split(' to ')
    start = sp[0]
    end = sp[1].split(' = ')[0]
    dist = int(sp[1].split(' = ')[1])

    if start not in travel:
        travel[start] = dict()
    if end not in travel:
        travel[end] = dict()

    travel[start][end] = dist
    travel[end][start] = dist


def journey(loc, to_visit):
    # If only one place left to visit, return distance to this point
    if len(to_visit) == 1:
        return travel[loc][to_visit[0]]

    min_dist = math.inf
    for new_loc in to_visit:
        new_v = [x for x in to_visit if x != new_loc]

        loc_dist = travel[loc][new_loc] if loc else 0
        dist = loc_dist + journey(new_loc, new_v)

        if dist < min_dist:
            min_dist = dist

    return min_dist


print(journey(None, list(travel.keys())))

# 16:46 (44th)

# Part 2

# 17:12 (42nd)
