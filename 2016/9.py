from Utils import *

# Part 1

inp = rd("9.txt")

decomp = ''

i = 0
while i < len(inp):
    c = inp[i]

    if c == '(':
        dim = inp[i:].split(')')[0].split('(')[1].split('x')
        nc = int(dim[0])
        rep = int(dim[1])

        # Find index of first character after closing bracket
        start_i = i + len(inp[i:].split(')')[0]) + 1
        phrase = inp[start_i:(start_i+nc)]

        decomp += phrase * rep
        i = start_i + nc - 1
    else:
        decomp += inp[i]
    i += 1

print(decomp, len(decomp))
print(len(decomp))

# 7:07 (10th)

# Part 2

# inp = 'X(8x2)(3x3)ABCY'


def decomp(s):
    dec = ''
    i = 0
    while i < len(s):
        c = s[i]

        if c == '(':
            dim = s[i:].split(')')[0].split('(')[1].split('x')
            nc = int(dim[0])
            rep = int(dim[1])

            # Find index of first character after closing bracket
            start_i = i + len(s[i:].split(')')[0]) + 1
            phrase = s[start_i:(start_i + nc)]

            if '(' in phrase:
                dec += decomp(phrase) * rep
            else:
                dec += phrase * rep
            i = start_i + nc - 1
        else:
            dec += s[i]
        i += 1

    return dec


res = decomp(inp)
print(len(res))

# 11:02 (7th)
