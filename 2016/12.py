from Utils import *

# Part 1

inp = rdl("12.txt")

reg = {'a': 0,
       'b': 0,
       'c': 1,
       'd': 0}

i = 0
while i < len(inp):
    line = inp[i]
    ss = line.split(' ')

    if ss[0] == 'cpy':
        if ss[1].isnumeric():
            reg[ss[2]] = int(ss[1])
        else:
            reg[ss[2]] = reg[ss[1]]
        i += 1
    elif ss[0] == 'inc':
        reg[ss[1]] += 1
        i += 1
    elif ss[0] == 'dec':
        reg[ss[1]] -= 1
        i += 1
    elif ss[0] == 'jnz':
        is_zero = False
        if ss[1].isnumeric():
            is_zero = int(ss[1]) == 0
        else:
            is_zero = reg[ss[1]] == 0

        if not is_zero:
            i += int(ss[2])
        else:
            i += 1

print(reg)

# 3:19 (1st)

# Part 2

# 3:43 (1st)