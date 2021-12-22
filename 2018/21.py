from Utils import *

# Part 1

inp = rdl("21.txt")


def run(reg, inp):
    v = set()
    ls = inp[0].split()
    bind = int(ls[1])
    ptr = reg[bind]

    iv = False

    k = 0
    while ptr < (len(inp) - 1):
        line = inp[ptr + 1]
        ls = line.split()
        instr = ls[0]

        a = int(ls[1])
        b = int(ls[2])
        c = int(ls[3])

        reg[bind] = ptr
        before = reg[2]
        b4 = reg[4]
        b1 = reg[1]

        if not iv and instr == 'addi' and a == 2 and b == 1 and c == 2 and reg[2] > 10:
            reg[5] = 100000
            iv = True

        if ptr == 28:
            if reg[2] not in v:
                v.add(reg[2])
                print(reg[2])

        if instr == 'addi':
            reg[c] = reg[a] + b
        elif instr == 'addr':
            reg[c] = reg[a] + reg[b]
        elif instr == 'mulr':
            reg[c] = reg[a] * reg[b]
        elif instr == 'muli':
            reg[c] = reg[a] * b
        elif instr == 'banr':
            reg[c] = reg[a] & reg[b]
        elif instr == 'bani':
            reg[c] = reg[a] & b
        elif instr == 'borr':
            reg[c] = reg[a] | reg[b]
        elif instr == 'bori':
            reg[c] = reg[a] | b
        elif instr == 'seti':
            reg[c] = a
        elif instr == 'setr':
            reg[c] = reg[a]
        elif instr == 'gtir':
            reg[c] = 1 if a > reg[b] else 0
        elif instr == 'gtri':
            reg[c] = 1 if reg[a] > b else 0
        elif instr == 'gtrr':
            reg[c] = 1 if reg[a] > reg[b] else 0
        elif instr == 'eqir':
            reg[c] = 1 if a == reg[b] else 0
        elif instr == 'eqri':
            reg[c] = 1 if reg[a] == b else 0
        elif instr == 'eqrr':
            reg[c] = 1 if reg[a] == reg[b] else 0

        ptr = reg[bind] + 1
        k += 1

        # print(before, line, " ".join([str(x) for x in reg]))

        if before != reg[2] or b4 != reg[4] and line != "addi 4 1 4" or b1 != reg[1]:
            print(line, before, reg[2], b4, reg[4], b1, reg[1])

    return True

# 10780777

# 33:44 (> 100th)

# run([0, 0, 0, 0, 0, 0], inp)

# Part 2: execute most instructions and then halt

v = [10780777]
b = 10780777
while True:
    a = b | 65536
    b = 1250634 + (a & 255)
    b *= 65899
    b &= 16777215
    a = a // 256
    c = a & 255
    b += c

    b *= 65899
    b &= 16777215
    a = a // 256
    c = a
    b += c
    b *= 65899
    b &= 16777215

    if b not in v:
        v.append(b)
    else:
        print(v[-1])

# 59:22 (93rd)




