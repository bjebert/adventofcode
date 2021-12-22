from Utils import *

# Part 1

inp = rdl("21.txt")

grid = [[".", "#", "."], [".", ".", "#"], ["#", "#", "#"]]

patterns = dict()

i = 0
for line in inp:
    ls = line.split(" => ")
    pat_in = [list(x) for x in ls[0].split("/")]
    pat_out = [list(x) for x in ls[1].split("/")]

    patterns[i] = (pat_in, pat_out)
    i += 1


def get_alternates(pat):
    out2 = []
    for z in range(4):
        out2.append(pat)
        out2.append([x[::-1] for x in pat])  # Flip

        new2 = []
        for y in range(len(pat)):
            new2.append([x[y] for x in pat][::-1])
        pat = new2

    return out2


for _ in range(18):
    size = len(grid)
    out = []
    enhance = []
    if size % 2 == 0:
        for x in range(0, size, 2):
            for y in range(0, size, 2):
                new = [grid[x][y:y+2], grid[x+1][y:y+2]]
                out.append(new)
    else:
        for x in range(0, size, 3):
            for y in range(0, size, 3):
                new = [grid[x][y:y+3], grid[x+1][y:y+3], grid[x+2][y:y+3]]
                out.append(new)

    for o in out:
        for p in patterns:
            if len(patterns[p][0]) == len(o):
                # Check for match
                pin = patterns[p][0]
                alts = get_alternates(pin)
                for a in alts:
                    if a == o:
                        enhance.append(patterns[p][1])
                        break

    if len(enhance) == 1:
        grid = enhance[0]
    else:
        grid = []
        dim = int(math.sqrt(len(enhance)))

        nr = len(enhance[0]) * dim

        z = 0
        y = 0
        for i in range(nr):
            row = []
            for j in range(dim):
                if len(enhance[j+y]) <= z:
                    print(z)
                row += enhance[j+y][z]
            grid.append(row)
            z += 1
            if z >= len(enhance[0]):
                z = 0
                y += dim

    count = []
    for x in grid:
        count.append(sum([y == '#' for y in x]))
    print(_, sum(count))

# 52:19 (> 100th)

# Part 2

# 54:45 (> 100th)

