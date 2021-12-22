from Utils import *

# Part 1

inp = rdl("3.txt")

gamma = ''
eps = ''
for i in range(len(inp[0])):
    c = [line[i] for line in inp]
    if c.count('0') > c.count('1'):
        gamma += '0'
        eps += '1'
    else:
        gamma += '1'
        eps += '0'

print(int(gamma, 2) * int(eps, 2))

# Part 2

filt_g = list(inp)
filt_e = list(inp)

gamma = ''
eps = ''
for i in range(len(inp[0])):
    g = [line[i] for line in filt_g]
    e = [line[i] for line in filt_e]

    if len(g) > 0:
        if g.count('0') > g.count('1'):
            gamma += '0'
            filt_g = [x for x in filt_g if x[i] == '0']
        else:
            gamma += '1'
            filt_g = [x for x in filt_g if x[i] == '1']

    if len(e) > 1:
        if e.count('0') <= e.count('1'):
            eps += '0'
            filt_e = [x for x in filt_e if x[i] == '0']
        else:
            eps += '1'
            filt_e = [x for x in filt_e if x[i] == '1']
    elif len(e) == 1:
        eps += filt_e[0][i:]
        filt_e = []


print(int(gamma, 2) * int(eps, 2))