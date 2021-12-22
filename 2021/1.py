from Utils import *

# Part 1

inp = rdl("1.txt")

inp = intlist(inp)

# Part 1

c = 0
for i in range(1, len(inp)):
    if inp[i] > inp[i-1]:
        c += 1

print(c)  # 0:37 (9th)

# Part 2

c = 0
for i in range(3, len(inp)):
    if inp[i] + inp[i-1] + inp[i-2] > (inp[i-1] + inp[i-2] + inp[i-3]):
        c += 1

print(c)  # 1:14 (4th)









