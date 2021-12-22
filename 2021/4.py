from Utils import *

# Part 1

inp = rdl("4.txt")

nums = [int(x) for x in inp[0].split(',')]

boards = []

for i in range(2, len(inp), 6):
    board = [x.split() for x in inp[i:(i+5)]]
    for j in range(len(board)):
        board[j] = [int(x) for x in board[j]]

    boards.append(board)

boards_original = copy.deepcopy(boards)


def bingo(nums, boards, boards_original):
    for n in nums:
        # set boards equal to -1
        for board in boards:
            for j in range(len(board)):
                board[j] = [-1 if x == n else x for x in board[j]]

        # check for bingo of -1
        to_remove = []

        for i in range(len(boards)):
            board = boards[i]
            # check row bingo
            for row in board:
                if sum(row) == -5:
                    to_remove.append(i)

            # columns
            for k in range(5):
                if sum([x[k] for x in board]) == -5:
                    to_remove.append(i)

        boards_new = []
        boards_new2 = []

        if len(boards) == 1 and len(to_remove):
            return boards_original[0], boards[0], n

        for j in range(len(boards)):
            if j not in to_remove:
                boards_new.append(boards[j])
                boards_new2.append(boards_original[j])

        boards = copy.deepcopy(boards_new)
        boards_original = copy.deepcopy(boards_new2)


winner = bingo(nums, boards, boards_original)

unmarked = winner[0]
marked = winner[1]
n = winner[2]

c = 0
for row in marked:
    c += sum([x for x in row if x != -1])

print(c*n)

