from Utils import *


loc = [8, 2]  # input
# loc = [4, 8]
cap = 21

# 1st on part 1!

@cache
def game(loc1, loc2, score1, score2, turn, rolls, roll_sum=0):
    if rolls == 0:
        if turn == 0:
            loc1 = ((loc1 + roll_sum - 1) % 10) + 1
            score1 += loc1
        else:
            loc2 = ((loc2 + roll_sum - 1) % 10) + 1
            score2 += loc2

        rolls = 3
        turn = (turn + 1) % 2
        roll_sum = 0

    if score1 >= cap:
        return [1, 0]
    elif score2 >= cap:
        return [0, 1]

    wins = [0, 0]
    for roll in (1, 2, 3):
        g = game(loc1, loc2, score1, score2, turn, rolls - 1, roll_sum + roll)
        wins[0] += g[0]
        wins[1] += g[1]

    return wins


print(max(game(loc[0], loc[1], 0, 0, 0, 3)))








