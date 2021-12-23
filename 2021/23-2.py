from Utils import *

inp = rdl("23a.txt")

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


goal_squares = {'A': [(3, 2), (3, 3), (3, 4), (3, 5)],
                'B': [(5, 2), (5, 3), (5, 4), (5, 5)],
                'C': [(7, 2), (7, 3), (7, 4), (7, 5)],
                'D': [(9, 2), (9, 3), (9, 4), (9, 5)]}


def get_neighbours(amp, score):
    cost = {'A': 1, 'B': 10, 'C': 100, 'D': 1000}
    hallway = [(1, 1), (2, 1), (4, 1), (6, 1), (8, 1), (10, 1), (11, 1)]

    neighbours = []
    n_goal = []

    blocked = []
    for k in letters:
        for x in amp[k]:
            blocked.append((x[0], x[1]))

    # blocking goal
    blocked_goal = dict()  # Is goal blocked by a different letter?
    for k in letters:
        blocked_goal[k] = False
        for k2 in amp:
            if k2 != k:
                for x in amp[k2]:
                    if (x[0], x[1]) in goal_squares[k]:
                        blocked_goal[k] = True

    for k in letters:
        for i in range(len(amp[k])):
            pod = amp[k][i]
            x, y = pod[0], pod[1]

            if (x, y) in goal_squares[k] and not blocked_goal[k]:
                continue

            end = []
            for j in range(3, -1, -1):
                if goal_squares[k][j] not in blocked and not blocked_goal[k]:
                    end.append(goal_squares[k][j])
                    break

            if pod[2] == 0:
                end += hallway

            for x2, y2 in end:
                if (x2, y2) != (x, y):

                    path = get_simple_path(x, y, x2, y2)

                    bad = False
                    for b in blocked:
                        if b in path[1:]:
                            bad = True
                            break

                    if path and not bad:
                        new_amp = copy.deepcopy(amp)
                        new_amp[k][i] = (x2, y2, 1)
                        neighbours.append((new_amp, score + (len(path) - 1) * cost[k]))

                        if (x2, y2) in goal_squares[k]:  # Moved into goal; append to special list, which will now be returned
                            n_goal.append(neighbours[-1])

    if len(n_goal):
        return n_goal
    else:
        return neighbours


def goal_reached(amp):
    for k in letters:
        if [(x[0], x[1]) for x in amp[k]] != goal_squares[k]:
            return False
    return True


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
