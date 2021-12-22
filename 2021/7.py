from Utils import *

# Part 1

inp = rd("7.txt")

# inp = "16,1,2,0,4,2,7,1,2,14"

inp = ints(inp)

c = 0
min_c = math.inf
for i in range(450, 600):
    c = 0
    for x in inp:
        f = 1
        while x != i:
            if x > i:
                x -= 1
            else:
                x += 1
            c += f
            f += 1

    if c < min_c:
        min_c = c
        print(min_c)

print(min_c)
