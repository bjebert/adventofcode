from Utils import *

# Part 1


def get_a(start):
    reg = {'a': start}
    inp = rdl("23.txt")
    i = 0
    while i < len(inp):
        line = inp[i]
        ss = line.split(' ')

        if reg['a'] == 5040:
            xxx = 1

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
    return reg['a']

# print(get_a(7))

# 10:00 (6th)

# Part 2


print(get_a(7))

# (manually debugged to work out the rule)
# get_a(n) = n! + 71*73

# 48:46 (T-71st)




