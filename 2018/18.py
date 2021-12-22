from Utils import *

# Part 1

inp = rdl("18.txt")

grid = parse_grid(inp)

for i in range(1000000000):
    new = copy.deepcopy(grid)

    for k in grid:
        x, y = k

        opn = 0
        trees = 0
        lumber = 0

        for x2 in range(x-1, x+2):
            for y2 in range(y-1, y+2):
                if (x2, y2) in grid and (x2 != x or y2 != y):
                    if grid[(x2, y2)] == '.':
                        opn += 1
                    elif grid[(x2, y2)] == '|':
                        trees += 1
                    elif grid[(x2, y2)] == '#':
                        lumber += 1

        if grid[k] == '.':
            if trees >= 3:
                new[k] = '|'
            else:
                new[k] = '.'

        elif grid[k] == '|':
            if lumber >= 3:
                new[k] = '#'
            else:
                new[k] = '|'

        elif grid[k] == '#':
            if lumber >= 1 and trees >= 1:
                new[k] = '#'
            else:
                new[k] = '.'

    grid = copy.deepcopy(new)

    o = list(grid.values()).count('.')
    t = list(grid.values()).count('|')
    l = list(grid.values()).count('#')

    if (i+1) % 100 == 0:
        print(i+1, t*l)

# 3:43 (1st)

print(1000000000 % 700)

# 11:58 (8th)
