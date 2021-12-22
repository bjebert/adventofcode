from Utils import *

# Part 1

inp = [int(x) for x in rdl("5.txt")]


i = 0
count = 0
while i >= 0 and i < len(inp):
    count += 1
    jmp = inp[i]

    if jmp >= 3:
        inp[i] -= 1
    else:
        inp[i] += 1

    i += jmp

print(count)

# 1:21 (2nd)

# 1:51 (2nd)

