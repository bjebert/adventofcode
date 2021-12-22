from Utils import *

# Part 1

inp = rdl("10.txt")
#
# inp = """position=< 9,  1> velocity=< 0,  2>
# position=< 7,  0> velocity=<-1,  0>
# position=< 3, -2> velocity=<-1,  1>
# position=< 6, 10> velocity=<-2, -1>
# position=< 2, -4> velocity=< 2,  2>
# position=<-6, 10> velocity=< 2, -2>
# position=< 1,  8> velocity=< 1, -1>
# position=< 1,  7> velocity=< 1,  0>
# position=<-3, 11> velocity=< 1, -2>
# position=< 7,  6> velocity=<-1, -1>
# position=<-2,  3> velocity=< 1,  0>
# position=<-4,  3> velocity=< 2,  0>
# position=<10, -3> velocity=<-1,  1>
# position=< 5, 11> velocity=< 1, -2>
# position=< 4,  7> velocity=< 0, -1>
# position=< 8, -2> velocity=< 0,  1>
# position=<15,  0> velocity=<-2,  0>
# position=< 1,  6> velocity=< 1,  0>
# position=< 8,  9> velocity=< 0, -1>
# position=< 3,  3> velocity=<-1,  1>
# position=< 0,  5> velocity=< 0, -1>
# position=<-2,  2> velocity=< 2,  0>
# position=< 5, -2> velocity=< 1,  2>
# position=< 1,  4> velocity=< 2,  1>
# position=<-2,  7> velocity=< 2, -2>
# position=< 3,  6> velocity=<-1, -1>
# position=< 5,  0> velocity=< 1,  0>
# position=<-6,  0> velocity=< 2,  0>
# position=< 5,  9> velocity=< 1, -2>
# position=<14,  7> velocity=<-2,  0>
# position=<-3,  6> velocity=< 2, -1>""".split('\n')


pos = dict()
vel = dict()

i = 0
for line in inp:
    ls = line.split(',')

    pos0 = int(ls[0].split('<')[-1].strip())
    pos1 = int(ls[1].split('>')[0].strip())

    vel0 = int(ls[1].split(' ')[-1].split('<')[-1])
    vel1 = int(ls[2].strip().split('>')[0])

    pos[i] = [pos0, pos1]
    vel[i] = [vel0, vel1]

    i += 1

j = 0
while True:
    min_x = min([x[0] for x in pos.values()])
    max_x = max([x[0] for x in pos.values()])
    min_y = min([x[1] for x in pos.values()])
    max_y = max([x[1] for x in pos.values()])

    dist = (max_x - min_x) + (max_y - min_y)


    for p in pos:
        pos[p][0] += vel[p][0]
        pos[p][1] += vel[p][1]

    j += 1

    if dist < 90:
        print(j)
        grid = []
        for y in range(min_y, max_y+1):
            grid.append([])
            for x in range(min_x, max_x+1):
                c = '#' if [x, y] in pos.values() else '.'
                grid[y - min_y].append(c)

        rotate(grid)

        cnt = "".join(["".join(x) for x in grid]).count('#')
        if cnt > 0:
            print('\n')
            print("\n".join(["".join(x) for x in grid]))
            print('\n')

# 24:16 (> 100th)

# 25:22 (> 100th)
