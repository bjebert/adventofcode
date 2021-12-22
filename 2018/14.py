from Utils import *

# Part 1

inp = 793031

board = [3, 7]
pos = [0, 1]

inpint = [int(x) for x in list(str(inp))]

while True:
    recipe = board[pos[0]] + board[pos[1]]
    recint = [int(x) for x in list(str(recipe))]

    for k in recint:
        board.append(k)

        if board[len(board)-len(inpint):] == inpint:
            print(len(board) - len(inpint))
            exit(1)

        if len(board) % 1000000 == 0:
            print(len(board))

    for j in range(2):
        steps = board[pos[j]] + 1
        pos[j] = (pos[j] + steps) % len(board)


# print("".join([str(x) for x in board[rec:(rec+10)]]))

# 7:50 (65th)

# 17:41 (81st)