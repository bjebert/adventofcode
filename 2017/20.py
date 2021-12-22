from Utils import *

# Part 1

inp = rdl("20.txt")

pos = dict()
vel = dict()
acc = dict()

for i in range(len(inp)):
    ls = inp[i].split(', ')

    ps = ls[0].split(',')

    p0 = int(ps[0].split('<')[1])
    p1 = int(ps[1])
    p2 = int(ps[2].split('>')[0])

    pos[i] = [p0, p1, p2]

    vs = ls[1].split(',')
    v0 = int(vs[0].split('<')[1])
    v1 = int(vs[1])
    v2 = int(vs[2].split('>')[0])

    vel[i] = [v0, v1, v2]

    acs = ls[2].split(',')
    a0 = int(acs[0].split('<')[1])
    a1 = int(acs[1])
    a2 = int(acs[2].split('>')[0])

    acc[i] = [a0, a1, a2]

t = 0
while True:
    for p in pos:
        vel[p][0] += acc[p][0]
        vel[p][1] += acc[p][1]
        vel[p][2] += acc[p][2]

        pos[p][0] += vel[p][0]
        pos[p][1] += vel[p][1]
        pos[p][2] += vel[p][2]

    dead = []
    for p1 in pos:
        for p2 in pos:
            if p1 != p2:
                if pos[p1][0] == pos[p2][0] and pos[p1][1] == pos[p2][1] and pos[p1][2] == pos[p2][2]:
                    dead.append(p1)
                    dead.append(p2)

    for d in unique(dead):
        del pos[d]

    print(len(pos))
    if t > 500:
        break

    t += 1

#
# closest = math.inf
# w = None
# for p in pos:
#     manh = sum([abs(x) for x in pos[p]])
#
#     if manh < closest:
#         closest = manh
#         w = p

# print(w)

# 5:38 (10th)

# Part 2

print(len(pos))

# 9:14 (6th)


