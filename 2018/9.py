players = 459
last = 72103*100

score = dict(zip(range(1, players+1), [0 for x in range(players)]))

###

marble = 1
player = 0
marbles = {0: [0, 0]}
curr = 0

while marble <= last:
    if marble % 23 != 0:
        nxt = marbles[curr][1]
        after = marbles[nxt][1]

        marbles[marble] = [nxt, after]
        marbles[nxt][1] = marble
        marbles[after][0] = marble

        curr = marble
    else:
        score[player+1] += marble
        for i in range(7):
            curr = marbles[curr][0]

        # Remove current marble
        score[player+1] += curr
        prev, nxt = marbles[curr]
        marbles[prev][1] = nxt
        marbles[nxt][0] = prev
        curr = nxt

    marble += 1
    player = (player + 1) % players

print(score)
print(max(score.values()))



