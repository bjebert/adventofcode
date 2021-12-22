from Utils import *

# Part 1

inp = rdl("1.txt")

c1 = 0
c2 = 0
for line in inp:
    mass = int(line)
    fuel = math.floor(mass / 3) - 2
    c1 += fuel
    c2 += fuel

    while fuel >= 9:
        fuel = math.floor(fuel / 3) - 2
        c2 += fuel

print(c1, c2)

