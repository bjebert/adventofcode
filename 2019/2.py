from Utils import *

# Part 1

inp = rd("2.txt")
instr = [int(x) for x in inp.split(',')]



def opcode(instr):
    i = 0
    while i < len(instr):
        if instr[i] == 1:
            instr[instr[i+3]] = instr[instr[i+1]] + instr[instr[i+2]]
            i += 4
        elif instr[i] == 2:
            instr[instr[i+3]] = instr[instr[i+1]] * instr[instr[i+2]]
            i += 4
        elif instr[i] == 99:
            break

    return instr

# 3:55 (T-11th)

for a in range(1000):
    for b in range(1000):
        instr = [int(x) for x in inp.split(',')]
        instr[1] = a
        instr[2] = b

        try:
            if opcode(instr)[0] == 19690720:
                print(100 * a + b)
                break
        except:
            pass

# 6:06 (7th(