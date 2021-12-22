from Read import *
import re
import math

# Part 1

inp = rdl("14.txt")

rein = dict()

for line in inp:
    ss = line.split(' ')
    deer = ss[0]
    speed = int(ss[3])
    dur_travel = int(ss[6])
    dur_rest = int(ss[-2])

    rein[deer] = speed, dur_travel, dur_rest


dist = dict()
for r in rein:
    dist[r] = 0

status = dict()
for r in rein:
    status[r] = ["active", rein[r][1]]

points = dict()
for r in rein:
    points[r] = 0

for i in range(1, 2504):
    for r in rein:
        if status[r][1] <= 0:  # change status
            if status[r][0] == "active":
                status[r][0] = "rest"
                status[r][1] = rein[r][2]
            else:
                status[r][0] = "active"
                status[r][1] = rein[r][1]

        if status[r][0] == "active":
            dist[r] += rein[r][0]

        status[r][1] -= 1

    # who is leading?
    winner = ""
    max_dist = 0

    for r in rein:
        if dist[r] > max_dist:
            winner = r
            max_dist = dist[r]

    points[winner] += 1

print(max(dist.values()))

# 5:43 (5th)

print(max(points.values()))

# 7:32 (2nd)

