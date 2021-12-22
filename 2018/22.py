from Utils import *

# Part 1


depth = 5913
tx, ty = 8, 701

# depth = 510
# tx, ty = 10, 10

grid = dict()
ero = dict()


def erosion(x, y):
    if (x, y) not in ero:
        ero[(x, y)] = (geo(x, y) + depth) % 20183
    return ero[(x, y)]


def geo(x, y):
    if (x, y) == (0, 0) or (x, y) == (tx, ty):
        return 0
    if y == 0:
        return x * 16807
    if x == 0:
        return y * 48271

    return erosion(x-1, y) * erosion(x, y-1)


def typ(x, y):
    t = erosion(x, y) % 3
    if t == 0:
        return '.'
    elif t == 1:
        return '='
    else:
        return '|'


for x in range(100):
    for y in range(1000):
        grid[(x, y)] = typ(x, y)

# 11:08 (41st)

# Part 2

def get_neighbours(x, y, equipped, mins):
    neighbours = []
    eqp = {'.': ('C', 'T'),
           '=': ('C', 'N'),
           '|': ('T', 'N')}

    # Change tools
    for tool in ('C', 'T', 'N'):
        if tool != equipped and tool in eqp[grid[(x, y)]]:
            neighbours.append(((x, y), tool, mins + 7))

    # Move
    for x2, y2 in [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]:
        if (x2, y2) in grid:
            tools = eqp[grid[(x2, y2)]]

            if equipped in tools:
                neighbours.append(((x2, y2), equipped, mins + 1))

    return neighbours


start = ((0, 0), "T", 0)
q = [[start]]
fastest = math.inf
route = dict()

while q:
    curr = q.pop(0)

    x, y = curr[-1][0]
    equipped = curr[-1][1]
    mins = curr[-1][2]

    if (x, y) == (tx, ty) and equipped == 'T':
        if mins < fastest:
            fastest = mins
            print(fastest)
            # for c in curr:
            #     print(c)

    for n in get_neighbours(x, y, equipped, mins):
        if n[:2] not in route or n[2] < route[n[:2]]:
            route[n[:2]] = n[2]
            n2 = list(curr)
            n2.append(n)
            q.append(n2)

# 39:00 (40th)
