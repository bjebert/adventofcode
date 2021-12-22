from Utils import *
from queue import PriorityQueue

# Part 1

inp = rdl("15.txt")

grid = parse_grid(inp, True)

xm = xmax(grid)
ym = ymax(grid)

for a in range(5):
    for b in range(5):
        if not (a == 0 and b == 0):
            for x in range(xm + 1):
                for y in range(ym + 1):
                    grid[a * (xm + 1) + x, b * (ym + 1) + y] = (((grid[x, y] + a + b) - 1) % 9) + 1

xm = max([k[0] for k in grid])
ym = max([k[1] for k in grid])


print(djikstra(grid, (0, 0), (xm, ym)))
