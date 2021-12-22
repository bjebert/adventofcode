from Utils import *

# Part 1

discs = {1: [17, 1], 2: [7, 0], 3: [19, 2], 4: [5, 0], 5: [3, 0], 6: [13, 5]}
discs = {1: [17, 1], 2: [7, 0], 3: [19, 2], 4: [5, 0], 5: [3, 0], 6: [13, 5], 7: [11, 0]}

# discs = {1: [5, 4], 2: [2, 1]}

time = 0
while True:
    if sum([(discs[d][1] + time + d) % discs[d][0] for d in discs]) == 0:
        print(time)
        break
    time += 1

# 5:48 (T-15th)

# Part 2

# 6:08 (7th)