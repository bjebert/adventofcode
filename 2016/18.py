from Utils import *

# Part 1

inp = "^..^^.^^^..^^.^...^^^^^....^.^..^^^.^.^.^^...^.^.^.^.^^.....^.^^.^.^.^.^.^.^^..^^^^^...^.....^....^."
grid = [inp]


def is_trap(x, y):
    if y > 0:
        left = grid[x-1][y-1]
    else:
        left = '.'

    center = grid[x-1][y]

    if y < len(inp) - 1:
        right = grid[x-1][y+1]
    else:
        right = '.'

    if left == '^' and center == '^' and right == '.':
        return True

    if center == '^' and right == '^' and left == '.':
        return True

    if left == '^' and center == '.' and right == '.':
        return True

    if right == '^' and center == '.' and left == '.':
        return True

    return False


rows = 400000
for x in range(1, rows):
    row = ''

    for y in range(len(inp)):
        if is_trap(x, y):
            row += '^'
        else:
            row += '.'

    grid.append(row)

print(sum([x.count('.') for x in grid]))

# 6:56 (17th)

# Part 2

# 7:25 (9th)



