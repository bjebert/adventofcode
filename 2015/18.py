from Read import *
import re
import math
import itertools

# Part 1

inp = rdl("18.txt")


def neigh_on(i, j):
    count = 0
    for x in range(i-1, i+2):
        for y in range(j-1, j+2):
            if 0 <= x < len(inp) and 0 <= y < len(inp):
                if x == i and y == j:
                    continue
                if inp[x][y] == "#":
                    count += 1

    return count


for iter in range(100):
    new_inp = inp.copy()
    for i in range(len(inp)):
        for j in range(len(inp[i])):
            neigh = neigh_on(i, j)

            if inp[i][j] == "#":
                if neigh not in [2, 3]:
                    new_inp[i] = new_inp[i][:j] + "." + new_inp[i][j+1:]
            else:
                if neigh == 3:
                    new_inp[i] = new_inp[i][:j] + "#" + new_inp[i][j+1:]

    inp = new_inp.copy()

print(sum([x.count('#') for x in inp]))

# 13:40 (28th)

# Part 2

inp = rdl("18.txt")

for iter in range(100):
    new_inp = inp.copy()
    new_inp[0] = "#" + new_inp[0][1:-1] + "#"
    new_inp[-1] = "#" + new_inp[-1][1:-1] + "#"

    for i in range(len(inp)):
        for j in range(len(inp[i])):
            neigh = neigh_on(i, j)

            if inp[i][j] == "#":
                if neigh not in [2, 3]:
                    new_inp[i] = new_inp[i][:j] + "." + new_inp[i][j+1:]
            else:
                if neigh == 3:
                    new_inp[i] = new_inp[i][:j] + "#" + new_inp[i][j+1:]

    new_inp[0] = "#" + new_inp[0][1:-1] + "#"
    new_inp[-1] = "#" + new_inp[-1][1:-1] + "#"
    inp = new_inp.copy()

print(sum([x.count('#') for x in inp]))

# 15:21 (25th)
