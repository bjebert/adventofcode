from Utils import *

# Part 1

inp = rdl("12.txt")

maze = dict()

for line in inp:
    l, r = line.split('-')
    if l not in maze:
        maze[l] = []
    if r not in maze:
        maze[r] = []

    maze[l].append(r)
    maze[r].append(l)

q = [['start']]

paths = []


while q:
    path = q.pop(0)
    curr = path[-1]

    if curr == 'end':
        # check for unique lowers
        paths.append(path)
        if len(paths) % 1000 == 0:
            print(len(paths))

        continue

    neighbours = maze[curr]

    for neighbour in neighbours:
        if neighbour in ['start', 'end'] and path.count(neighbour) == 1:
            continue
        elif neighbour == neighbour.lower() and path.count(neighbour) >= 2:
            continue

        one = False
        bad = False
        for x in unique(path):
            if x == x.lower() and x not in ['start', 'end'] and path.count(x) >= 2:
                if one:
                    bad = True
                one = True

        if not bad:
            new_path = list(path)
            new_path.append(neighbour)
            q.append(new_path)

print(len(paths))