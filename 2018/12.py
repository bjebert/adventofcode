from Utils import *

# Part 1

inp = rdl("12.txt")

pots = dict()

i = 0
initial = inp[0].split(': ')[1]
for i in range(len(initial)):
    pots[i] = initial[i]

growth = dict()

for line in inp[2:]:
    ls = line.split(' => ')
    growth[ls[0]] = ls[1]

for i in range(20):
    # Pad either side with .
    min_p = min(pots.keys())
    max_p = max(pots.keys())

    for j in range(1, 6):
        pots[min_p - j] = '.'
        pots[max_p + j] = '.'

    new_pots = copy.deepcopy(pots)

    for p in range(min_p - 3, max_p + 4):
        seq = pots[p-2] + pots[p-1] + pots[p] + pots[p+1] + pots[p+2]

        if seq in growth:
            new_pots[p] = growth[seq]

    pots = copy.deepcopy(new_pots)

    count = 0
    for p in pots:
        if pots[p] == '#':
            count += p

    print(i, count)


# 8:58 (21st)

iterations = 50000000000
print(6728+32*(iterations-200))

# 11:57 (7th)

