from Utils import *

# Part 1

inp = 1352
dim = 100

grid = [[0 for x in range(dim)] for y in range(dim)]

for x in range(dim):
    for y in range(dim):
        xy_sum = x*x + 3*x + 2*x*y + y + y*y + inp
        one_sum = "{0:b}".format(xy_sum).count('1')

        if one_sum % 2 == 0:
            grid[y][x] = '.'
        else:
            grid[y][x] = '#'

for i in grid:
    print(' '.join(i))

# Manual solution
# 8:53 (12th)

# Part 2

# max 50 steps

def get_neighbours(loc):
    max_bound = 100
    neighbours = []

    for x, y in [(loc[0] + 1, loc[1]), (loc[0] - 1, loc[1]), (loc[0], loc[1] + 1), (loc[0], loc[1] - 1)]:
        if x >= 0 and x < max_bound:
            if y >= 0 and y < max_bound:
                if not (x == loc[0] and y == loc[1]):
                    if grid[y][x] == ".":
                        neighbours.append((x, y))

    return neighbours


def bfs():
    visited = set()
    start = (1, 1)
    queue = [[start]]
    visited.add(start)

    while queue:
        path = queue.pop(0)
        loc = path[-1]

        if len(path) <= 50:
            neighbours = get_neighbours(loc)

            for neighbour in neighbours:
                if neighbour not in visited:
                    visited.add(neighbour)
                    new_path = list(path)
                    new_path.append(neighbour)
                    queue.append(new_path)

    print(len(visited))

bfs()

# 19:11 (34th)


