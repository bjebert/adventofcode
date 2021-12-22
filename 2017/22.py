from Utils import *

# Part 1

inp = rdl("22.txt")
grid = dict()
#
# inp = """..#
# #..
# ...""".split('\n')

for x in range(len(inp)):
    for y in range(len(inp[x])):
        grid[(x, y)] = inp[x][y]


def print_grid(grid):
    min_x, max_x, min_y, max_y = math.inf, -math.inf, math.inf, -math.inf
    for k in grid:
        if k[0] < min_x:
            min_x = k[0]
        if k[0] > max_x:
            max_x = k[0]
        if k[1] < min_y:
            min_y = k[1]
        if k[1] > max_y:
            max_y = k[1]

    lst = []
    for x in range(min_x, max_x + 1):
        lst.append([])
        for y in range(min_y, max_y + 1):
            if (x, y) in grid:
                lst[x - min_x].append(grid[(x, y)])
            else:
                lst[x - min_x].append('.')

    print("\n".join(["".join(x) for x in lst]))


direc = 'U'
start = int((int(math.sqrt(len(grid.keys()))) / 2) - 0.5)
pos = [start, start]
infects = 0


for i in range(10000000):
    # print('\n' + str(i) + '\n')
    # print_grid(grid)
    x, y = pos[0], pos[1]

    if (x, y) not in grid:
        grid[(x, y)] = '.'

    if grid[(x, y)] == '#':
        # turn right
        if direc == 'U':
            direc = 'R'
        elif direc == 'R':
            direc = 'D'
        elif direc == 'D':
            direc = 'L'
        elif direc == 'L':
            direc = 'U'
    elif grid[(x, y)] == '.':
        # turn left
        if direc == 'U':
            direc = 'L'
        elif direc == 'R':
            direc = 'U'
        elif direc == 'D':
            direc = 'R'
        elif direc == 'L':
            direc = 'D'
    elif grid[(x, y)] == 'F':
        # reverse
        if direc == 'U':
            direc = 'D'
        elif direc == 'R':
            direc = 'L'
        elif direc == 'D':
            direc = 'U'
        elif direc == 'L':
            direc = 'R'

    if grid[(x, y)] == '#':
        grid[(x, y)] = 'F'
    elif grid[(x, y)] == 'W':
        infects += 1
        grid[(x, y)] = '#'
    elif grid[(x, y)] == '.':
        grid[(x, y)] = 'W'
    elif grid[(x, y)] == 'F':
        grid[(x, y)] = '.'

    if direc == 'U':
        pos = [x-1, y]
    elif direc == 'D':
        pos = [x+1, y]
    elif direc == 'L':
        pos = [x, y-1]
    elif direc == 'R':
        pos = [x, y+1]


print(infects)

# 21:08 (> 100th)

# Part 2

# 27:02 (> 100th)