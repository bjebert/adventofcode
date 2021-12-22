from Utils import *

# Part 1

xmin = 81
xmax = 129
ymin = -150
ymax = -108

c = 0

for vx in range(0, 500):
    for vy in range(-151, 201):
        max_y = -math.inf
        x, y = 0, 0

        i = 0
        target = False

        sx, sy = vx, vy

        while True:
            x += sx
            y += sy
            if sx > 0:
                sx -= 1
            elif sx < 0:
                sx += 1
            else:
                sx = 0
            sy -= 1

            if xmin <= x <= xmax and ymin <= y <= ymax:
                target = True
                c += 1
                break

            i += 1
            if i > 500:
                break

print(c)