from Utils import *

# Part 1

chk = 12134527
state = 'A'
line = defaultdict(int)
pos = 0

for i in range(chk):
    if line[pos] == 0:
        line[pos] = 1

        if state == 'A':
            pos += 1
            state = 'B'
        elif state == 'B':
            pos -= 1
            state = 'A'
        elif state == 'C':
            pos += 1
            state = 'A'
        elif state == 'D':
            pos -= 1
            state = 'E'
        elif state == 'E':
            pos += 1
            state = 'F'
        elif state == 'F':
            pos += 1
            state = 'A'

    elif line[pos] == 1:
        if state == 'A':
            line[pos] = 0
            pos -= 1
            state = 'C'
        elif state == 'B':
            line[pos] = 1
            pos += 1
            state = 'C'
        elif state == 'C':
            line[pos] = 0
            pos -= 1
            state = 'D'
        elif state == 'D':
            line[pos] = 1
            pos -= 1
            state = 'C'
        elif state == 'E':
            line[pos] = 1
            pos += 1
            state = 'A'
        elif state == 'F':
            line[pos] = 1
            pos += 1
            state = 'E'

print(sum(list(line.values())))

# 4:08 (2nd)

# Part 2

# 4:10 (2nd)