from Utils import *

# Part 1

grid = rdl("24.txt")
i = 1

# grid: 39 rows x 179 cols



# grid = """###########
# #0.1.....2#
# #.#######.#
# #4.......3#
# ###########""".split('\n')


def get_moves(curr, nr, nc, targets):
    x, y = curr[0]

    moves = []

    for x2, y2 in [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]:
        if 0 <= x2 <= nr and 0 <= y2 <= nc:
            if grid[x2][y2] == ".":
                moves.append(([x2, y2], curr[1]))
            elif grid[x2][y2].isnumeric():
                target = int(grid[x2][y2])

                acq = list(curr[1])
                if target not in acq:
                    if target != 0:
                        acq.append(target)
                    elif target == 0 and len(acq) == targets:
                        acq.append(0)

                    acq.sort()

                moves.append(([x2, y2], acq))

    return moves


def bfs():
    nr, nc = len(grid), len(grid[0])

    if nr == 5 and nc == 11:
        targets = 4
    else:
        targets = 7

    start_row = [x for x, y in enumerate(grid) if '0' in y][0]
    start_col = [x for x, y in enumerate(grid[start_row]) if y == '0'][0]

    start_pos = ([start_row, start_col], [])
    queue = [[start_pos]]
    visited = set()

    i = 0
    while queue:
        path = queue.pop(0)
        curr = path[-1]

        if i % 1000 == 0:
            print(i, len(path), len(queue))

        if len(curr[1]) == targets + 1:
            return path

        moves = get_moves(curr, nr, nc, targets)

        for m in moves:
            if json.dumps(m) not in visited:
                visited.add(json.dumps(m))
                new_path = list(path)
                new_path.append(m)
                queue.append(new_path)

        i += 1

    return 'No path found'


b = bfs()
print(len(b) - 1)  # 442

# 16:29 (16th)

# Part 2

# Can pick up 0 if have all other targets

# 24:19 (28th)
