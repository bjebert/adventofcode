from Read import *

inp = rdl("7.txt")

# Part 1

wires = dict()
processed = [False for line in inp]


def process_line(sender):
    if sender.isnumeric():
        return int(sender)
    else:
        ss = sender.split(' ')

        if len(ss) == 1:
            if ss[0] not in wires:
                return None

            return wires[sender] & 0xFFFF
        elif len(ss) == 2:
            if ss[1] not in wires:
                return None

            return ~wires[ss[1]] & 0xFFFF
        else:
            wireL = ss[0]
            op = ss[1]
            wireR = ss[2]

            if wireL.isnumeric():
                lval = int(wireL)
            else:
                if wireL not in wires:
                    return None
                lval = wires[wireL]

            if op == 'AND':
                if wireR not in wires:
                    return None

                return lval & wires[wireR] & 0xFFFF
            elif op == 'OR':
                if wireR not in wires:
                    return None

                return lval | wires[wireR] & 0xFFFF
            elif op == 'LSHIFT':
                return lval << int(wireR) & 0xFFFF
            elif op == 'RSHIFT':
                return lval >> int(wireR) & 0xFFFF


while not all(processed):
    for k in [i for i, j in enumerate(processed) if not j]:
        line = inp[k]
        instr = line.split(' -> ')

        signal = process_line(instr[0])
        receiver = instr[1]

        if signal is not None:
            wires[receiver] = signal
            processed[k] = True


print(wires['a'])

# 24:55 (13th)

# Part 2

wires = dict()
inp[inp.index('1674 -> b')] = '46065 -> b'
processed = [False for line in inp]

while not all(processed):
    for k in [i for i, j in enumerate(processed) if not j]:
        line = inp[k]
        instr = line.split(' -> ')

        signal = process_line(instr[0])
        receiver = instr[1]

        if signal is not None:
            wires[receiver] = signal
            processed[k] = True


print(wires['a'])

# 29:46 (21st)