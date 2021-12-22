from Utils import *

# Part 1

inp = rdl("11.txt")

grid = parse_grid(inp, True)

flashes = 0
last = 0

i = 0
while True:
    for k in grid:
        grid[k] += 1

    f = set()

    while any([x > 9 for x in grid.values()]):
        for k in grid:
            if grid[k] > 9:
                flashes += 1
                x, y = k
                f.add((x, y))

                for x2 in range(x - 1, x + 2):
                    for y2 in range(y - 1, y + 2):
                        if (x2, y2) in grid and not (x2 == x and y2 == y):
                            if (x2, y2) not in f:
                                grid[(x2, y2)] += 1

                grid[k] = 0

    i += 1

    if i == 100:
        print(flashes)

    if flashes - last == 100:
        print(i)
        exit()

    last = flashes

