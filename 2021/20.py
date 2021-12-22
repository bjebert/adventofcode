from Utils import *

inp = rdl("20.txt")

algo = inp[0]
grid = parse_grid(inp[2:])

# print_grid(grid)

for i in range(50):
    new_grid = copy.deepcopy(grid)
    a, b = xmin(grid), xmax(grid)
    c, d = ymin(grid), ymax(grid)

    for x in range(a - 1, b + 2):
        for y in range(c - 1, d + 2):
            s = ''
            for x2, y2 in [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]:
                if (x2, y2) not in grid:
                    if i % 2 == 0:
                        grid[(x2, y2)] = '.'
                    else:
                        grid[(x2, y2)] = '#'

                s += grid[(x2, y2)]

            s = s.replace('#', '1')
            s = s.replace('.', '0')
            num = int(s, 2)
            new_grid[(x, y)] = algo[num]

    grid = copy.deepcopy(new_grid)

print("".join(grid.values()).count('#'))  # 5259 right
