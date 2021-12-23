from Utils import *

inp = rdl("23.txt")

grid = parse_grid(inp)

amp = defaultdict(list)

for k in grid:
    if grid[k] in ['A', 'B', 'C', 'D']:
        z = list(k)
        z.append(0)
        amp[grid[k]].append(tuple(z))
        grid[k] = '.'

letters = ['A', 'B', 'C', 'D']


def print_amp(amp):
    tmp = copy.deepcopy(grid)
    for k in letters:
        for x in amp[k]:
            tmp[(x[0], x[1])] = k

    print_grid(tmp)


def amp2tuple(amp, score):
    z = []

    for k in letters:
        x = amp[k]
        x.sort()
        z.append((k, tuple(x)))

    return tuple(z), score


def tuple2amp(tup):
    amp = defaultdict(list)

    for k in tup:
        amp[k[0]] = list(k[1])

    return amp


@cache
def get_simple_path(x, y, x_goal, y_goal):
    v = set()
    start = (x, y)
    v.add(start)
    q = [[start]]

    best_path = None

    while q:
        curr = q.pop(0)
        x, y = curr[-1]

        if x == x_goal and y == y_goal:
            if not best_path:
                best_path = curr
            else:
                if len(curr) < len(best_path):
                    best_path = curr

        neighbours = []

        # ADJACENT (U, L, R, D)
        for x2, y2 in [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]:
            if (x2, y2) in grid and grid[(x2, y2)] == '.':
                neighbours.append((x2, y2))

        for n in neighbours:
            if n not in v:
                v.add(n)
                new = list(curr)
                new.append(n)
                q.append(new)

    return best_path


def get_neighbours(amp, score):

    neighbours = []
    cost = {'A': 1, 'B': 10, 'C': 100, 'D': 1000}

    blocked = []
    for k in letters:
        for x in amp[k]:
            blocked.append((x[0], x[1]))

    goal_squares = {'A': [(3, 2), (3, 3)],
                    'B': [(5, 2), (5, 3)],
                    'C': [(7, 2), (7, 3)],
                    'D': [(9, 2), (9, 3)]}

    forbidden = defaultdict(list)
    for k in letters:
        for k2 in letters:
            if k2 != k:
                for x in goal_squares[k2]:
                    forbidden[k].append(x)

    end_squares = [(1, 1), (2, 1), (4, 1), (6, 1), (8, 1), (10, 1), (11, 1),
                   (3, 2), (5, 2), (7, 2), (9, 2),
                   (3, 3), (5, 3), (7, 3), (9, 3)]  # Includes hallway

    for k in letters:
        for i in range(len(amp[k])):
            pod = amp[k][i]
            x, y = pod[0], pod[1]
            if pod[2] == 1:
                end = goal_squares[k]

                # If bottom goal square is vacant, then must move there
                if end[1] not in blocked:
                    end = [end[1]]

            else:
                end = end_squares

            for x2, y2 in end:
                if (x2, y2) not in forbidden[k]:
                    if (x2, y2) != (x, y):

                        path = get_simple_path(x, y, x2, y2)

                        if path and not any([b in path[1:] for b in blocked]):
                            new_amp = copy.deepcopy(amp)
                            new_amp[k][i] = (x2, y2, 1)
                            neighbours.append((new_amp, score + (len(path) - 1) * cost[k]))

    return neighbours


def goal_reached(amp):
    if [(x[0], x[1]) for x in amp['A']] == [(3, 2), (3, 3)]:
        if [(x[0], x[1]) for x in amp['B']] == [(5, 2), (5, 3)]:
            if [(x[0], x[1]) for x in amp['C']] == [(7, 2), (7, 3)]:
                if [(x[0], x[1]) for x in amp['D']] == [(9, 2), (9, 3)]:
                    return True

    return False


start = amp2tuple(amp, 0)
scores = dict()
scores[start[0]] = start[1]
q = [[start]]

best_score = math.inf

while q:
    curr = q.pop(0)
    last = curr[-1]
    amp = tuple2amp(last[0])
    score = last[1]

    neighbours = get_neighbours(amp, score)

    if goal_reached(amp):
        if score < best_score:
            best_score = score
            print(best_score)

            for c in curr:
                print('---')
                print_amp(tuple2amp(c[0]))

    for n in neighbours:
        tup = amp2tuple(n[0], n[1])

        if tup[0] not in scores or (tup[0] in scores and tup[1] < scores[tup[0]]):
            scores[tup[0]] = tup[1]

            new = list(curr)
            new.append(tup)
            q.append(new)
