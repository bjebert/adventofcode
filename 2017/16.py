from Utils import *

# Part 1

inp = rd("16.txt").strip().split(',')
line = 'abcdefghijklmnop'


def dance(line):
    for instr in inp:
        ss = instr.split('/')

        if ss[0][0] == 's':
            amt = int(ss[0][1:])
            line = line[(len(line)-amt):] + line[:(len(line) - amt)]

        elif ss[0][0] == 'x':
            a = int(ss[0][1:])
            b = int(ss[1])

            la = line[a]
            lb = line[b]

            line = line.replace(la, '_')
            line = line.replace(lb, la)
            line = line.replace('_', lb)

        elif ss[0][0] == 'p':
            la = ss[0][1]
            lb = ss[1]

            line = line.replace(la, '_')
            line = line.replace(lb, la)
            line = line.replace('_', lb)

    return line

# print(dance('abcdefghijklmnop'))

# 8:14 (87th)

line = 'abcdefghijklmnop'
for i in range(1000000000 % 60):
    # print(i, [x for x, y in enumerate(line) if y == 'a'][0])
    line = dance(line)

print(line)

# a: 60

# 17:43 (45th)
