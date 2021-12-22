from Utils import *

# Part 1

inp = "udskfozm"



loc = [0, 0]
goal = [3, 3]

dir_map = {0: 'U', 1: 'D', 2: 'L', 3: 'R'}


def bfs(passcode):
    longest = 0
    queue = [[([0, 0], [])]]

    while queue:
        path = queue.pop(0)
        curr = path[-1]

        loc = curr[0]

        if loc == [3, 3]:
            path_length = min([x for x, y in enumerate(path) if y[0] == [3, 3]])
            if path_length > longest:
                longest = path_length
                print(longest)
                continue

        doors = md5(passcode + "".join(curr[1]))[:4]

        status = []
        for d in doors:
            if d in ['b', 'c', 'd', 'e', 'f']:
                status.append(1)
            else:
                status.append(0)

        directions = []

        for i in range(4):
            if status[i] == 1:
                if dir_map[i] == 'U' and loc[1] > 0:
                    directions.append('U')
                elif dir_map[i] == 'D' and loc[1] < 3:
                    directions.append('D')
                elif dir_map[i] == 'L' and loc[0] > 0:
                    directions.append('L')
                elif dir_map[i] == 'R' and loc[0] < 3:
                    directions.append('R')

        for d in directions:
            new_path = list(path)

            if d == 'U':
                new_loc = [loc[0], loc[1] - 1]
            elif d == 'D':
                new_loc = [loc[0], loc[1] + 1]
            elif d == 'L':
                new_loc = [loc[0] - 1, loc[1]]
            elif d == 'R':
                new_loc = [loc[0] + 1, loc[1]]

            new_moves = list(curr[1])
            new_moves.append(d)

            new_path.append((new_loc, new_moves))
            queue.append(new_path)

    return "NO PATH"

print(bfs('kglvqrro'))

# 17:10 (83rd)

# Part 2

# 22:58 (74th)

