from Utils import *

# Part 1

inp = rdl("19.txt")

# inp = """     |
#      |  +--+
#      A  |  C
#  F---|--|-E---+
#      |  |  |  D
#      +B-+  +--+ """.split('\n')

grid = [list(x) for x in inp]

pos = [0, [x for x, y in enumerate(grid[0]) if y == '|'][0]]
direc = 'D'

letters = []
steps = 1
while True:
    x, y = pos
    steps += 1

    if direc == 'L':
        if y > 0 and grid[x][y-1] != ' ':
            pos = [x, y-1]
            if grid[x][y-1] not in ['|', '-', '+']:
                letters.append(grid[x][y-1])
        else:
            steps -= 1
            # Change direction
            if x-1 >= 0 and grid[x-1][y] != ' ':
                direc = 'U'
            elif x+1 < len(grid) and grid[x+1][y] != ' ':
                direc = 'D'
            else:
                break

    elif direc == 'R':
        if y+1 < len(grid[0]) and grid[x][y+1] != ' ':
            pos = [x, y+1]
            if grid[x][y+1] not in ['|', '-', '+']:
                letters.append(grid[x][y+1])
        else:
            steps -= 1
            # Change direction
            if x-1 >= 0 and grid[x-1][y] != ' ':
                direc = 'U'
            elif x+1 < len(grid) and grid[x+1][y] != ' ':
                direc = 'D'
            else:
                break

    elif direc == 'U':
        if x > 0 and grid[x-1][y] != ' ':
            pos = [x-1, y]
            if grid[x-1][y] not in ['|', '-', '+']:
                letters.append(grid[x-1][y])
        else:
            steps -= 1
            # Change direction
            if y-1 >= 0 and grid[x][y-1] != ' ':
                direc = 'L'
            elif y+1 < len(grid[0]) and grid[x][y+1] != ' ':
                direc = 'R'
            else:
                break

    elif direc == 'D':
        if x+1 < len(grid) and grid[x+1][y] != ' ':
            pos = [x+1, y]
            if grid[x+1][y] not in ['|', '-', '+']:
                letters.append(grid[x+1][y])
        else:
            steps -= 1
            # Change direction
            if y-1 >= 0 and grid[x][y-1] != ' ':
                direc = 'L'
            elif y+1 < len(grid[0]) and grid[x][y+1] != ' ':
                direc = 'R'
            else:
                break

print("".join(letters))
print(steps)

# 8:57 (T-15th)

# Part 2

# 19:57 (> 100th)