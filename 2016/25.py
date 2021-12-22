from Utils import *

# Part 1


def clocks(a, ret):
    inp = rdl("25.txt")
    i = 0
    reg = {'a': a}
    clock = []
    while i < len(inp) and len(clock) < ret:
        line = inp[i]
        ss = line.split(' ')

        if ss[0] == 'cpy':
            if ss[1].strip('-').isnumeric():
                reg[ss[2]] = int(ss[1])
            else:
                if ss[2] in ['a', 'b', 'c', 'd']:
                    reg[ss[2]] = reg[ss[1]]
            i += 1
        elif ss[0] == 'inc':
            reg[ss[1]] += 1
            i += 1
        elif ss[0] == 'dec':
            reg[ss[1]] -= 1
            i += 1
        elif ss[0] == 'jnz':
            if ss[1].strip('-').isnumeric():
                is_zero = int(ss[1]) == 0
            else:
                is_zero = reg[ss[1]] == 0

            if not is_zero:
                if ss[2].strip('-').isnumeric():
                    i += int(ss[2])
                else:
                    i += reg[ss[2]]
            else:
                i += 1
        elif ss[0] == 'tgl':
            if ss[1].isnumeric():
                x = int(ss[1])
            else:
                x = reg[ss[1]]

            idx = i + x
            if not (idx >= len(inp) or idx < 0):
                tgl_line = inp[idx]
                ss2 = tgl_line.split(' ')
                if ss2[0] == 'inc':
                    ss2[0] = 'dec'
                elif ss2[0] == 'dec':
                    ss2[0] = 'inc'
                elif ss2[0] == 'jnz':
                    ss2[0] = 'cpy'
                elif ss2[0] == 'cpy':
                    ss2[0] = 'jnz'
                elif ss2[0] == 'tgl':
                    ss2[0] = 'inc'

                inp[idx] = " ".join(ss2)
            i += 1
        elif ss[0] == 'out':
            clock.append(reg[ss[1]])
            i += 1

    return clock


i = 0
while True:
    c = clocks(i, ret=10)
    if len(unique(c[::2])) == 1 and len(unique(c[1::2])) == 1:
        print(i)
        break
    i += 1

# 6:47 (13th)

# Part 2

# 6:52 (11th)

