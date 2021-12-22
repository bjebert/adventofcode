from Utils import *

# Part 1

inp = rdl("18.txt")

reg = dict()

for line in inp:
    ls = line.split()

    if ls[1] not in reg and not ls[1].isnumeric():
        reg[ls[1]] = 0


freq = None
i = 0

while i < len(inp):
    ls = inp[i].split()

    if ls[1].strip('-').isnumeric():
        a = int(ls[1])
    else:
        a = reg[ls[1]]

    if ls[0] == 'set':
        if ls[2].strip('-').isnumeric():
            b = int(ls[2])
        else:
            b = reg[ls[2]]

        reg[ls[1]] = b

    elif ls[0] == 'mul':
        if ls[2].strip('-').isnumeric():
            b = int(ls[2])
        else:
            b = reg[ls[2]]

        reg[ls[1]] *= b
    elif ls[0] == 'jgz':
        if a > 0:
            if ls[2].strip('-').isnumeric():
                b = int(ls[2])
            else:
                b = reg[ls[2]]

            i += b
            i -= 1

    elif ls[0] == 'snd':
        freq = a
    elif ls[0] == 'add':
        if ls[2].strip('-').isnumeric():
            b = int(ls[2])
        else:
            b = reg[ls[2]]

        reg[ls[1]] += b
    elif ls[0] == 'mod':
        if ls[2].strip('-').isnumeric():
            b = int(ls[2])
        else:
            b = reg[ls[2]]

        reg[ls[1]] %= b
    elif ls[0] == 'rcv':
        if a != 0:
            print(freq)
            break

    i += 1

# 6:39 (T-15th)

# Part 1

inp = rdl("18.txt")
# inp = """snd 1
# snd 2
# snd p
# rcv a
# rcv b
# rcv c
# rcv d""".split('\n')

reg1 = dict()
reg2 = dict()

for line in inp:
    ls = line.split()

    if ls[1] not in reg1 and not ls[1].strip('-').isnumeric():
        reg1[ls[1]] = 0
        reg2[ls[1]] = 0

reg2['p'] = 1


def program(instr, i, reg, q):
    out = []
    sent = False
    count = 0

    while i < len(instr):
        ls = instr[i].split()

        if ls[1].strip('-').isnumeric():
            a = int(ls[1])
        else:
            a = reg[ls[1]]

        if ls[0] == 'set':
            if ls[2].strip('-').isnumeric():
                b = int(ls[2])
            else:
                b = reg[ls[2]]

            reg[ls[1]] = b

        elif ls[0] == 'mul':
            if ls[2].strip('-').isnumeric():
                b = int(ls[2])
            else:
                b = reg[ls[2]]

            reg[ls[1]] *= b
        elif ls[0] == 'jgz':
            if a > 0:
                if ls[2].strip('-').isnumeric():
                    b = int(ls[2])
                else:
                    b = reg[ls[2]]
                i += (b-1)

        elif ls[0] == 'snd':
            out.append(a)
            sent = True
            count += 1
        elif ls[0] == 'add':
            if ls[2].strip('-').isnumeric():
                b = int(ls[2])
            else:
                b = reg[ls[2]]

            reg[ls[1]] += b
        elif ls[0] == 'mod':
            if ls[2].strip('-').isnumeric():
                b = int(ls[2])
            else:
                b = reg[ls[2]]

            reg[ls[1]] %= b
        elif ls[0] == 'rcv':
            if not len(q):
                return i, reg, out, sent, count
            else:
                reg[ls[1]] = q.pop(0)

        i += 1

    return i, reg, out, sent, count

instr1 = list(inp)
instr2 = list(inp)
i1 = 0
i2 = 0
q1 = []
q2 = []

sent1 = True
sent2 = True

count = 0
while sent1 or sent2:
    i1, reg1, q2, sent1, count1 = program(instr1, i1, reg1, q1)
    i2, reg2, q1, sent2, count2 = program(instr2, i2, reg2, q2)

    count += count2

print(count)

# 23:12 (23rd)