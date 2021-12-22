import regex as re
import math
from itertools import permutations, combinations, product
from functools import reduce, cache
from collections import defaultdict
import hashlib
import copy
import json
from timeit import timeit
import csv
from queue import PriorityQueue


### Parsing input


def ints(inp):
    return [int(x) for x in inp.split(',')]


def parse_grid(inp, as_numeric=False):
    grid = dict()

    for y in range(len(inp)):
        for x in range(len(inp[y])):
            grid[(x, y)] = inp[y][x]

    if as_numeric:
        for k in grid:
            grid[k] = int(grid[k])

    return grid


### Reading input


def rd(filename):  # Read
    with open(filename, 'r') as f:
        return f.read().replace('\n', '')


def rdl(filename):  # Read lines
    with open(filename, 'r') as f:
        return [line.replace('\n', '') for line in f.readlines()]

###


def sort_dict(dic, asc=True):  # Sort a dictionaries keys (by ascending if asc=True, else descending)
    return dict(sorted(dic.items(), key=lambda x: x[1], reverse=not asc))


def unique(lst):  # Reduce a list to its unique values
    return reduce(lambda l, x: l.append(x) or l if x not in l else l, lst, [])


def md5(s):  # Get the MD5 hash of a string s
    return hashlib.md5(s.encode('utf-8')).hexdigest()


def coords2grid(coords):  # Convert list/tuple of [x, y] coordinates to a 2D grid
    x_val = [c[0] for c in coords]
    y_val = [c[1] for c in coords]

    grid = []
    for y in range(min(y_val), max(y_val) + 1):
        grid.append([])
        for x in range(min(x_val), max(x_val) + 1):
            c = '#' if [x, y] in coords else '.'
            grid[y - min(y_val)].append(c)

    return grid


def print_grid(grid):  # Print out a 2D grid line-by-line
    if type(grid) in (dict, defaultdict):
        return print_grid(dict2grid(grid))

    print("\n".join(["".join([str(y) for y in x]) for x in grid]))


def dict2grid(d):  # Return a grid from a dictionary containing 2D coords as keys
    coords = d.keys()
    x_val = [c[0] for c in coords]
    y_val = [c[1] for c in coords]

    grid = []
    for y in range(min(y_val), max(y_val) + 1):
        grid.append([])
        for x in range(min(x_val), max(x_val) + 1):
            if (x, y) in d:
                grid[y - min(y_val)].append(d[(x, y)])
            else:
                grid[y - min(y_val)].append('.')

    return grid


def dimensions(grid):
    return xmin(grid), xmax(grid), ymin(ymin), ymax(ymax)


def xmin(grid):
    return min([k[0] for k in grid])


def xmax(grid):
    return max([k[0] for k in grid])


def ymin(grid):
    return min([k[1] for k in grid])


def ymax(grid):
    return max([k[1] for k in grid])


def rotate(grid):
    return [[x[y] for x in grid][::-1] for y in range(len(grid))]


def flip(grid):
    return [x[::-1] for x in grid]


def write_grid(grid):
    with open("out.csv", "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerows(grid)


def reading_order(lst):  # Sort list of 2D coordinates in reading order (top-to-bottom, left-to-right)
    lst.sort(key=lambda x: (x[1], x[0]))


# Same as BFS but we use a priority queue to determine which node to visit next (one with lowest current distance to reach)
def djikstra(grid, start, end):
    v = set()
    pq = PriorityQueue()
    pq.put((0, start))

    dist = defaultdict(lambda: math.inf)
    dist[start] = 0

    while not pq.empty():
        score, (x, y) = pq.get()
        v.add((x, y))

        for x2, y2 in [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]:
            if (x2, y2) in grid:

                if (x2, y2) not in v:
                    cost = grid[(x2, y2)]  # Cost of travelling to x2, y2

                    if dist[(x, y)] + cost < dist[(x2, y2)]:
                        dist[(x2, y2)] = dist[(x, y)] + cost
                        pq.put((dist[(x2, y2)], (x2, y2)))

    return dist[end]
