from Utils import *

# Part 1

inp = rdl("8.txt")

print(inp)

# inp = ['rect 3x2',
#        'rotate column x=1 by 1',
#        'rotate row y=0 by 4',
#        'rotate column x=1 by 1']

grid = dict()

pix_w = 50
pix_h = 6

for x in range(pix_h):
    for y in range(pix_w):
        grid[(x, y)] = 0

for line in inp:
    ss = line.split(' ')

    if ss[0] == 'rect':
        dim = ss[1].split('x')
        w = int(dim[0])
        h = int(dim[1])

        for x in range(h):
            for y in range(w):
                grid[(x, y)] = 1

    elif ss[0] == 'rotate':
        which = int(ss[2].split('=')[1])
        amt = int(ss[-1])

        old_grid = grid.copy()

        if ss[1] == 'row':
            for x in range(pix_w):
                grid[(which, x)] = old_grid[(which, (x - amt) % pix_w)]
        elif ss[1] == 'column':
            for x in range(pix_h):
                grid[(x, which)] = old_grid[((x - amt) % pix_h, which)]

print(sum(grid.values()))

# 9:31 (9th)

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns

df = pd.DataFrame({'x': [k[0] for k in grid],
                   'y': [k[1] for k in grid],
                   'z': [grid[k] for k in grid]})

data = df.pivot(index='x', columns='y', values='z')
sns.heatmap(data)
plt.show()

# 32:21 (> 100th)