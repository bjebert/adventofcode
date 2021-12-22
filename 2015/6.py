# Part 1

# with open('6.txt', 'r') as f:
#     inp = [line.replace('\n', '') for line in f.readlines()]
#
#     # inp = ['turn on 0,0 through 999,999',
#     #        'turn off 499,499 through 500,500']
#
#     grid = dict()
#     for i in range(1000):
#         for j in range(1000):
#             grid[(i, j)] = 0
#
#     for line in inp:
#         if line[:6] == 'toggle':
#             start = [int(x) for x in line.split(' ')[1].split(',')]
#             end = [int(x) for x in line.split(' ')[3].split(',')]
#
#             for x in range(start[0], end[0]+1):
#                 for y in range(start[1], end[1]+1):
#                     if grid[(x, y)] == 0:
#                         grid[(x, y)] = 1
#                     else:
#                         grid[(x, y)] = 0
#
#         elif line[:8] == 'turn off':
#             start = [int(x) for x in line.split(' ')[2].split(',')]
#             end = [int(x) for x in line.split(' ')[4].split(',')]
#
#             for x in range(start[0], end[0]+1):
#                 for y in range(start[1], end[1]+1):
#                     grid[(x, y)] = 0
#
#         elif line[:7] == 'turn on':
#             start = [int(x) for x in line.split(' ')[2].split(',')]
#             end = [int(x) for x in line.split(' ')[4].split(',')]
#
#             for x in range(start[0], end[0]+1):
#                 for y in range(start[1], end[1]+1):
#                     grid[(x, y)] = 1
#
#     print(sum(list(grid.values())))

# 8:47 (7th)

# Part 2

with open('6.txt', 'r') as f:
    inp = [line.replace('\n', '') for line in f.readlines()]

    # inp = ['turn on 0,0 through 999,999',
    #        'turn off 499,499 through 500,500']

    grid = dict()
    for i in range(1000):
        for j in range(1000):
            grid[(i, j)] = 0

    for line in inp:
        if line[:6] == 'toggle':
            start = [int(x) for x in line.split(' ')[1].split(',')]
            end = [int(x) for x in line.split(' ')[3].split(',')]

            for x in range(start[0], end[0]+1):
                for y in range(start[1], end[1]+1):
                    grid[(x, y)] += 2

        elif line[:8] == 'turn off':
            start = [int(x) for x in line.split(' ')[2].split(',')]
            end = [int(x) for x in line.split(' ')[4].split(',')]

            for x in range(start[0], end[0]+1):
                for y in range(start[1], end[1]+1):
                    grid[(x, y)] = max(grid[(x, y)] - 1, 0)

        elif line[:7] == 'turn on':
            start = [int(x) for x in line.split(' ')[2].split(',')]
            end = [int(x) for x in line.split(' ')[4].split(',')]

            for x in range(start[0], end[0]+1):
                for y in range(start[1], end[1]+1):
                    grid[(x, y)] += 1

    print(sum(list(grid.values())))

# 9:51 (3rd)
