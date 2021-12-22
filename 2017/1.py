from Utils import *

# Part 1

inp = rd("1.txt")

count = 0
for i in range(len(inp)):
    j = int((i + len(inp) / 2) % len(inp))
    if inp[i] == inp[j]:
        count += int(inp[i])

print(count)

# 1:54 (15th)

# Part 2

# 3:04 (16th)
