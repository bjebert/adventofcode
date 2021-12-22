from Utils import *

# Part 1

inp = 348
buf = [0]
pos = 0

while len(buf) <= 2017:
    pos = (pos + inp) % len(buf)
    buf.insert(pos + 1, len(buf))
    pos = pos + 1

    if len(buf) % 100000 == 0:
        print(len(buf))

print(buf[[x for x, y in enumerate(buf) if y == 2017][0] + 1])

# 4:57 (34th)

inp = 348
pos = 0
pos_zero = 0
after = 0

for i in range(50000000):
    pos = ((pos + inp) % (i + 1)) + 1

    if pos <= pos_zero:
        pos_zero += 1

    if pos == pos_zero + 1:
        after = i + 1

print(after)

# 56:52 (> 100th)
