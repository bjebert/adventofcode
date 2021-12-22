from Utils import *

# Part 1

inp = "106,16,254,226,55,2,1,166,177,247,93,0,255,228,60,36".split(',')
inp = [int(x) for x in inp]

# inp = [3, 4, 1, 5]
lst = [i for i in range(256)]

pos = 0
skip = 0

for i in inp:
    if i > 1:
        sub = lst[pos:(pos+i)]
        idx = list(range(pos, pos+i))[:len(sub)]

        if pos+i > len(lst):
            sub += lst[:(pos+i) % len(lst)]
            idx += list(range((pos+i) % len(lst)))

        rev = sub[::-1]
        for x in range(len(idx)):
            lst[idx[x]] = rev[x]

    pos = (pos + i + skip) % len(lst)
    skip += 1

print(lst[0] * lst[1])

# 16:07 (> 100th)

# Part 2

inp = "106,16,254,226,55,2,1,166,177,247,93,0,255,228,60,36"

inp_conv = [ord(x) for x in inp] + [17, 31, 73, 47, 23]
list_size = 256

pos = 0
skip = 0

lst = [i for i in range(list_size)]
for _ in range(64):
    for i in inp_conv:
        if i > 1:
            sub = lst[pos:(pos+i)]
            idx = list(range(pos, pos+i))[:len(sub)]

            if pos+i > len(lst):
                sub += lst[:(pos+i) % len(lst)]
                idx += list(range((pos+i) % len(lst)))

            rev = sub[::-1]
            for x in range(len(idx)):
                lst[idx[x]] = rev[x]

        pos = (pos + i + skip) % len(lst)
        skip += 1


bits = []
for j in range(0, 256, 16):
    bits.append(reduce(lambda a, b: a ^ b, lst[j:(j+16)]))

unpadded = [str(hex(x)).split('x')[1] for x in bits]
padded = ['0' + x if len(x) == 1 else x for x in unpadded]

print("".join(padded))

# 44:10 (> 100th)