from Utils import *

# Part 1

inp = rdl("10.txt")
# inp = """value 5 goes to bot 2
# bot 2 gives low to bot 1 and high to bot 0
# value 3 goes to bot 1
# bot 1 gives low to output 1 and high to bot 0
# bot 0 gives low to output 2 and high to output 0
# value 2 goes to bot 2""".split('\n')

bots = dict()
output = dict()

# Assign bots
for line in inp:
    ss = line.split(' ')
    if ss[0] == 'bot':
        bot_num = int(ss[1])
        low = int(ss[6])
        low_bot = ss[5] == 'bot'

        hi = int(ss[-1])
        hi_bot = ss[-2] == 'bot'

        if not low_bot:
            output[low] = None
        if not hi_bot:
            output[hi] = None

        lb = "BOT" if low_bot else "OUTPUT"
        hb = "BOT" if hi_bot else "OUTPUT"

        bots[bot_num] = [False, [], lb, low, hb, hi]


# Give values
for line in inp:
    ss = line.split(' ')
    if ss[0] == 'value':
        val = int(ss[1])
        receiver = int(ss[5])

        bots[receiver][1].append(val)
        bots[receiver][1].sort()


while any([not bots[k][0] for k in bots]):
    for k in bots:
        if len(bots[k][1]) == 2:
            bots[k][0] = True

            if bots[k][1][0] == 17 and bots[k][1][1] == 61:
                print(k)

            if bots[k][2] == 'BOT':
                bots[bots[k][3]][1].append(bots[k][1][0])
                bots[bots[k][3]][1].sort()
            elif bots[k][2] == 'OUTPUT':
                output[bots[k][3]] = bots[k][1][0]

            bots[k][1].remove(bots[k][1][0])

            max_ix = len(bots[k][1]) - 1
            if bots[k][4] == 'BOT':
                bots[bots[k][5]][1].append(bots[k][1][max_ix])
                bots[bots[k][5]][1].sort()
            elif bots[k][4] == 'OUTPUT':
                output[bots[k][5]] = bots[k][1][max_ix]

            bots[k][1].remove(bots[k][1][max_ix])


# 17:04 (T-28th)

# Part 2

print(output[0] * output[1] * output[2])

# 17:23 (17th)