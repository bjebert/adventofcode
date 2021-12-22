from Utils import *

# Part 1

inp = rdl("9.txt")
# inp = """2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678""".split('\n')

grid = parse_grid(inp)

# Find low points

lows = set()


for k in grid:
    is_low = True
    for coord in [(k[0] + 1, k[1]), (k[0] - 1, k[1]), (k[0], k[1] + 1), (k[0], k[1] - 1)]:
        if coord in grid:
            if int(grid[coord]) <= int(grid[k]):
                is_low = False

    if is_low:
        lows.add(k)

c = 0
for l in lows:
    c += int(grid[l]) + 1

print(c)

# for each low, find how many points will move towards that low

basins = dict()
for l in lows:
    basins[l] = 0

for l in grid:
    q = [l]
    v = set()

    if int(grid[l]) == 9:
        continue

    while q:
        k = q.pop(0)

        if k in lows:
            basins[k] += 1
            break

        neighbours = [(k[0] + 1, k[1]), (k[0] - 1, k[1]), (k[0], k[1] + 1), (k[0], k[1] - 1)]

        for neighbour in neighbours:
            if neighbour not in v and neighbour in grid and int(grid[neighbour]) < int(grid[k]):
                v.add(neighbour)
                q.append(neighbour)

print(basins)
print(sorted(list(basins.values()), reverse=True)[:3])

print(94*93*90)





