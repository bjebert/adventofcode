from Utils import *

# Part 1

inp = rdl("5.txt")

grid = defaultdict(int)

for line in inp:
    ls = line.split(' -> ')
    start = [int(x) for x in ls[0].split(',')]
    end = [int(x) for x in ls[1].split(',')]

    if start[0] == end[0]:
        if start[1] > end[1]:
            for i in range(end[1], start[1] + 1):
                grid[start[0], i] += 1
        else:
            for i in range(start[1], end[1] + 1):
                grid[start[0], i] += 1
    elif start[1] == end[1]:
        if start[0] > end[0]:
            for i in range(end[0], start[0] + 1):
                grid[i, start[1]] += 1
        else:
            for i in range(start[0], end[0] + 1):
                grid[i, start[1]] += 1
    else:
        sx = min(start[0], end[0])
        ex = max(start[0], end[0])

        sy = min(start[1], end[1])
        ey = max(start[1], end[1])

        if start[0] < end[0] and start[1] < end[1]:
            for i in range(ex - sx + 1):
                grid[start[0] + i, start[1] + i] += 1
        elif start[0] < end[0] and start[1] > end[1]:
            for i in range(ex - sx + 1):
                grid[start[0] + i, start[1] - i] += 1
        elif start[0] > end[0] and start[1] > end[1]:
            for i in range(ex - sx + 1):
                grid[start[0] - i, start[1] - i] += 1
        else:
            for i in range(ex - sx + 1):
                grid[start[0] - i, start[1] + i] += 1


c = 0
for k in grid:
    if grid[k] > 1:
        c += 1

print(c)
# print_grid(dict2grid(grid))