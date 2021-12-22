from Utils import *

# Part 1

inp = [[int(y) for y in x.split('\t')] for x in rdl("2.txt")]

print(inp)

# count = 0
# for i in inp:
#     count += max(i) - min(i)
#
# print(count)

# 1:11 (12th)

count = 0
for i in inp:
    for j in range(len(i)):
        for k in range(len(i)):
            if j != k and i[j] / i[k] % 1 == 0:
                count += int(i[j] / i[k])

print(count)

# 2:52 (T-13th)

