from Utils import *

# Part 1

inp = rd("20.txt")

# inp = '^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$'

adj = defaultdict(list)
q = [(1, (0, 0), inp[1:-1], None)]
v = set()

while q:
    i, (x, y), path, end = q.pop(0)

    # 1. Step through path until we find a non-direction
    j = None
    for j in range(len(path)):
        c = path[j]
        x1, y1 = x, y
        if c == 'N':
            y -= 1
        elif c == 'S':
            y += 1
        elif c == 'E':
            x += 1
        elif c == 'W':
            x -= 1
        else:
            break

        adj[(x1, y1)].append((x, y))
        adj[(x, y)].append((x1, y1))

    i += j if j else 0

    if not len(path):
        rem = 'X'
    else:
        rem = path[j:]

    neighbours = []

    # 2a. If next character is an open bracket, generate neighbours until we reach the closed bracket
    if rem[0] == '(':
        lvl = 0
        neighbours.append([i+1, '', None])
        for k in range(len(rem)):
            c = rem[k]
            if c == '(':
                lvl += 1
            elif c == ')':
                lvl -= 1
                if lvl == 0:
                    break
            elif c == '|' and lvl == 1:
                neighbours.append([i+k+1, '', None])
            if c != '|' or lvl > 1:
                if not (c == '(' and lvl == 1):
                    neighbours[len(neighbours) - 1][1] += c

        for n in neighbours:
            n[2] = i + k + 1

    elif rem[0] == '$':
        continue

    # 2b. If next character is not a bracket, step to end point.
    else:
        if end is None:
            continue
        neighbours.append([end, inp[end:], end + len(inp[end:])])

    nc = sum([len(x[1]) for x in neighbours]) + len(neighbours) + 1
    end = i + nc
    for neigh in neighbours:
        node = (neigh[0], (x, y), neigh[1], neigh[2])
        if node not in v:
            v.add(node)
            q.append(node)

# Longest shortest path

print('Adjacency complete')

start = (0, 0)
q = [[start]]
rooms = dict()

while q:
    path = q.pop(0)
    curr = path[-1]

    if curr in rooms and len(path) - 1 < rooms[curr]:
        rooms[curr] = len(path) - 1
    else:
        rooms[curr] = len(path) - 1

    for n in adj[curr]:
        if n not in path:
            new = list(path)
            new.append(n)
            q.append(new)

print(max(rooms.values()))
print(sum([x >= 1000 for x in rooms.values()]))

# > 3h on both parts
