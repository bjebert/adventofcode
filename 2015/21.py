from Read import *
import re
import math
import itertools

# Part 1


def is_win(hp, dmg, armor):
    i = 0
    while True:
        if i % 2 == 0:  # Our turn
            dealt = max(1, dmg[0] - armor[1])
            hp[1] -= dealt

            if hp[1] <= 0:
                return True

        else:  # Boss turn
            dealt = max(1, dmg[1] - armor[0])
            hp[0] -= dealt

            if hp[0] <= 0:
                return False

        i += 1

# guess and check

# 7:07 (2nd)

# most amt of gold and still lose?

weapons = [(8, 4, 0), (10, 5, 0), (25, 6, 0), (40, 7, 0), (74, 8, 0)]
armor = [(0, 0, 0), (13, 0, 1), (31, 0, 2), (53, 0, 3), (75, 0, 4), (102, 0, 5)]
rings = [(0, 0, 0), (25, 1, 0), (50, 2, 0), (100, 3, 0), (20, 0, 1), (40, 0, 2), (80, 0, 3)]

max_cost = 0
for w in weapons:
    for a in armor:
        for r1 in rings:
            for r2 in rings:
                if r1 != r2:
                    cost = w[0] + a[0] + r1[0] + r2[0]
                    dm = w[1] + a[1] + r1[1] + r2[1]
                    defens = w[2] + a[2] + r1[2] + r2[2]

                    if not is_win([100, 100], [dm, 8], [defens, 2]):
                        if cost > max_cost:
                            print(cost)
                            max_cost = cost
                            print(w, a, r1, r2)

# 15:24 (5th)





