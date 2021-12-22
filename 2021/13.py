from Utils import *

# Part 1

inp = rdl("13.txt")

grid = dict()

folds = []
for line in inp:
    ls = line.split(',')

    if len(ls) > 1:
        x = int(ls[0])
        y = int(ls[1])

        grid[x, y] = '#'
    else:
        ls = line.split()
        if len(ls) > 2:
            fold = ls[2].split('=')

            fold_v = int(fold[1])
            folds.append((fold[0], fold_v))


max_x = max([k[0] for k in grid])
max_y = max([k[1] for k in grid])

for f in folds:
    if f[0] == 'x':
        i = 1
        for x in range(f[1] + 1, max_x + 1):
            for y in range(0, max_y + 1):
                if (x, y) in grid and grid[(x, y)] == '#':
                    grid[f[1] - i, y] = '#'

                grid[(x, y)] = '.'
            i += 1
    else:
        i = 1
        for y in range(f[1] + 1, max_y + 1):
            for x in range(0, max_x + 1):
                if (x, y) in grid and grid[(x, y)] == '#':
                    grid[x, f[1] - i] = '#'

                grid[(x, y)] = '.'
            i += 1

        # del all values

to_del = []
for k in grid:
    if grid[k] == '.':
        to_del.append(k)

for k in to_del:
    del grid[k]

print_grid(grid)






