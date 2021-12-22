from Utils import *

# Part 1

inp = rd("6.txt")

# inp = '3,4,3,1,2'
inp = [int(x) for x in inp.split(',')]

fish = dict()
for i in range(9):
    fish[i] = inp.count(i)


for i in range(256):
    new = copy.deepcopy(fish)

    for j in range(0, 8):
        new[j] = fish[j+1]

    new[8] = fish[0]
    new[6] += fish[0]

    fish = copy.deepcopy(new)

    if i == 79 or i == 255:
        print(sum(fish.values()))
