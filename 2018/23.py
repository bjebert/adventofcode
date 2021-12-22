from Utils import *

# Part 1

inp = rdl("23.txt")
# inp = """pos=<10,12,12>, r=2
# pos=<12,14,12>, r=2
# pos=<16,12,12>, r=4
# pos=<14,14,14>, r=6
# pos=<50,50,50>, r=200
# pos=<10,10,10>, r=5""".split('\n')

radius = dict()

for line in inp:
    ls = line.split()
    r = int(ls[1].split('=')[1])

    a, b, c = ls[0].split(',')[:3]
    x = int(a.split('<')[1])
    y = int(b)
    z = int(c.split('>')[0])

    radius[(x, y, z)] = r


strongest = None
mc = 0

for n in radius:
    c = 0
    for n2 in radius:
        if abs(n2[0] - n[0]) + abs(n2[1] - n[1]) + abs(n2[2] - n[2]) <= radius[n]:
            c += 1

    if c > mc:
        mc = c
        strongest = n

# 13:44

def in_range(x, y, z, x2, y2, z2, r):
    return abs(x2 - x) + abs(y2 - y) + abs(z2 - z) <= r

# Part 2

# Could we do a grid search?

min_x = min([k[0] for k in radius.keys()])
max_x = max([k[0] for k in radius.keys()])
min_y = min([k[1] for k in radius.keys()])
max_y = max([k[1] for k in radius.keys()])
min_z = min([k[2] for k in radius.keys()])
max_z = max([k[2] for k in radius.keys()])


def all_in_range(x, y, z):
    c = 0
    for n in radius:
        c += int(in_range(x, y, z, n[0], n[1], n[2], radius[n]))

    return c

print(all_in_range(26563629, 44410821, 39645652))
print(26563629 + 44410821 + 39645652)
exit()

max_c = 0
k = None
for x in range(27145279, 27145280, 1):
    for y in range(42410821, 46410821, 10000):
        for z in range(37645652, 41645652, 100000):
            c = all_in_range(x, y, z)

            if c >= max_c:
                max_c = c
                k = (x, y, z)
                print(k, max_c)

# 38:05 (13th)

# Brute force
# max_c = 0
# k = None
# for x in range(10, 51):
#     for y in range(10, 51):
#         for z in range(10, 51):
#             c = 0
#             for n in radius:
#                 c += int(in_range(x, y, z, n[0], n[1], n[2], radius[n]))
#
#             if c > max_c:
#                 max_c = c
#                 k = (x, y, z)
#                 print(k, max_c)