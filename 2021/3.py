from Utils import *

# Part 1

# 2:18 (20th)

# Part 2

inp = rdl("3.txt")

st = ""
for i in range(len(inp[0])):
    cnt = []

    for line in inp:
        cnt += str(line[i])

    if len(cnt):
        if cnt.count('0') > cnt.count('1'):
            inp = [x for x in inp if x[i] == '0']
            st += '0'
        else:
            inp = [x for x in inp if x[i] == '1']
            st += '1'

print(int(st, 2))

inp = rdl("3.txt")

ep = ""
for i in range(len(inp[0])):
    cnt = []

    for line in inp:
        cnt += str(line[i])

    if len(cnt) == 1:
        ep += line[i:]
        break

    if len(cnt):
        if cnt.count('0') <= cnt.count('1'):
            inp = [x for x in inp if x[i] == '0']
            ep += '0'
        else:
            inp = [x for x in inp if x[i] == '1']
            ep += '1'

print(int(st, 2) * int(ep, 2))

# 10:05 (91st)
