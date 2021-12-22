from Utils import *

# Part 1

inp = rdl("16.txt")

part1 = inp[:3160]
part2 = inp[3162:]


def program(reg, instr):
    A = instr[1]
    B = instr[2]
    C = instr[3]
    
    if instr[0] == 0:
        reg[C] = reg[A] + reg[B]
    elif instr[0] == 1:
        reg[C] = reg[A] + B
    elif instr[0] == 2:  # mulr
        reg[C] = reg[A] * reg[B]
    elif instr[0] == 3:  # muli
        reg[C] = reg[A] * B
    elif instr[0] == 4:  # banr
        reg[C] = reg[A] & reg[B]
    elif instr[0] == 5:  # bani
        reg[C] = reg[A] & B
    elif instr[0] == 6:  # borr
        reg[C] = reg[A] | reg[B]
    elif instr[0] == 7:  # bori
        reg[C] = reg[A] | B
    elif instr[0] == 8:  # setr
        reg[C] = reg[A]
    elif instr[0] == 9:  # seti
        reg[C] = A
    elif instr[0] == 10:  # gtir
        reg[C] = 1 if A > reg[B] else 0
    elif instr[0] == 11:
        reg[C] = 1 if reg[A] > B else 0
    elif instr[0] == 12:  # gtrr
        reg[C] = 1 if reg[A] > reg[B] else 0
    elif instr[0] == 13:  # eqir
        reg[C] = 1 if A == reg[B] else 0
    elif instr[0] == 14:
        reg[C] = 1 if reg[A] == B else 0
    elif instr[0] == 15:
        reg[C] = 1 if reg[A] == reg[B] else 0

    return reg

g_count = 0
opmap = dict()
for i in range(0, len(part1), 4):
    before = [int(x) for x in part1[i].split('[')[1].split(']')[0].split(', ')]
    instr = [int(x) for x in part1[i+1].split(' ')]
    after = [int(x) for x in part1[i+2].split('[')[1].split(']')[0].split(', ')]

    count = 0
    original = instr[0]
    for op in range(16):
        instr[0] = op
        reg = list(before)

        if program(reg, instr) == after:
            count += 1
            if original not in opmap:
                opmap[original] = []
            if op not in opmap[original]:
                opmap[original].append(op)

    if count >= 3:
        g_count += 1

# print(g_count)

# 18:20 (70th)

while any([len(x) > 1 for x in list(opmap.values())]):
    for op in opmap:
        if len(opmap[op]) == 1:
            val = opmap[op][0]
            for op2 in opmap:
                if op2 != op:
                    if val in opmap[op2]:
                        opmap[op2].remove(val)


for op in opmap:
    opmap[op] = opmap[op][0]

reg = [0, 0, 0, 0]

for line in part2:
    instr = [int(x) for x in line.split(' ')]
    instr[0] = opmap[instr[0]]

    reg = program(reg, instr)

print(reg)

# 24:47 (26th)