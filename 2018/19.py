from Utils import *

# Part 1

inp = rdl("19.txt")

reg = [1, 0, 0, 0, 0, 0]

ls = inp[0].split()
bind = int(ls[1])
ptr = reg[bind]

iv = False

while ptr < (len(inp) - 1):
    line = inp[ptr+1]
    ls = line.split()
    instr = ls[0]

    a = int(ls[1])
    b = int(ls[2])
    c = int(ls[3])

    reg[bind] = ptr
    before = ptr

    if not iv and instr == 'addi' and a == 2 and b == 1 and c == 2 and reg[2] > 10:
        reg[5] = 100000
        iv = True

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
    print(before, line, " ".join([str(x) for x in reg]))

print(reg)

# 19:54 (> 100th)

# Part 2

c = 0
for i in range(1, int(math.ceil(math.sqrt(10551261)))):
    if 10551261 % i == 0:
        c += i
        c += 10551261 / i

print(c)

# 1:00:18 (98th)
