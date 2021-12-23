from Utils import *

inp = rdl("22.txt")

grid = defaultdict(int)

for line in inp:
    l, r = line.split()

    co = [x.split('..') for x in r.split(',')]
    x0 = int(co[0][0].split('=')[1])
    x1 = int(co[0][1])

    y0 = int(co[1][0].split('=')[1])
    y1 = int(co[1][1])

    z0 = int(co[2][0].split('=')[1])
    z1 = int(co[2][1])

    for x in range(max(-50, x0), min(50, x1 + 1)):
        for y in range(max(-50, y0), min(50, y1 + 1)):
            for z in range(max(-50, z0), min(50, z1 + 1)):
                if l == 'on':
                    grid[(x, y, z)] = 1
                else:
                    grid[(x, y, z)] = 0

print(sum(grid.values()))

boundaries = []
c = 0


def get_overlap(status, x0, x1, y0, y1, z0, z1, boundaries):
    if len(boundaries) == 0:
        return 0

    overlap_sum = 0
    for i in range(len(boundaries)):
        b = boundaries[i]

        o_x0, o_x1 = None, None
        o_y0, o_y1 = None, None
        o_z0, o_z1 = None, None

        if x0 > b['x1'] or x1 < b['x0']:
            pass  # No overlap
        elif x0 >= b['x0'] and x1 <= b['x1']:  # Envelops
            o_x0 = x0
            o_x1 = x1
        elif x0 <= b['x0'] and x1 >= b['x1']:  # Enveloped
            o_x0 = b['x0']
            o_x1 = b['x1']
        elif x0 >= b['x0']:  # Overlaps to the right (+x)
            o_x0 = x0
            o_x1 = b['x1']
        elif x1 >= b['x0']:  # Overlaps to the left (-x)
            o_x0 = b['x0']
            o_x1 = x1

        if y0 > b['y1'] or y1 < b['y0']:
            pass  # No overlap
        elif y0 >= b['y0'] and y1 <= b['y1']:  # Envelops
            o_y0 = y0
            o_y1 = y1
        elif y0 <= b['y0'] and y1 >= b['y1']:  # Enveloped
            o_y0 = b['y0']
            o_y1 = b['y1']
        elif y0 >= b['y0']:  # Overlaps to the right (+y)
            o_y0 = y0
            o_y1 = b['y1']
        elif y1 >= b['y0']:  # Overlaps to the left (-y)
            o_y0 = b['y0']
            o_y1 = y1

        if z0 > b['z1'] or z1 < b['z0']:
            pass  # No overlap
        elif z0 >= b['z0'] and z1 <= b['z1']:  # Envelops
            o_z0 = z0
            o_z1 = z1
        elif z0 <= b['z0'] and z1 >= b['z1']:  # Enveloped
            o_z0 = b['z0']
            o_z1 = b['z1']
        elif z0 >= b['z0']:  # Overlaps to the right (+z)
            o_z0 = z0
            o_z1 = b['z1']
        elif z1 >= b['z0']:  # Overlaps to the left (-z)
            o_z0 = b['z0']
            o_z1 = z1

        if o_x0 is not None and o_y0 is not None and o_z0 is not None:
            overlap_sum += (o_x1 - o_x0 + 1) * (o_y1 - o_y0 + 1) * (o_z1 - o_z0 + 1)
            overlap_sum -= get_overlap(status, o_x0, o_x1, o_y0, o_y1, o_z0, o_z1, boundaries[(i + 1):])

    return overlap_sum


for line in inp[::-1]:
    status, r = line.split()

    co = [x.split('..') for x in r.split(',')]
    x0 = int(co[0][0].split('=')[1])
    x1 = int(co[0][1])

    y0 = int(co[1][0].split('=')[1])
    y1 = int(co[1][1])

    z0 = int(co[2][0].split('=')[1])
    z1 = int(co[2][1])

    cubes = (x1 - x0 + 1) * (y1 - y0 + 1) * (z1 - z0 + 1)
    overlap = get_overlap(status, x0, x1, y0, y1, z0, z1, boundaries)

    if status == 'on':
        c += cubes - overlap

    boundaries.append({'status': status, 'x0': x0, 'x1': x1, 'y0': y0, 'y1': y1, 'z0': z0, 'z1': z1})

print(c)