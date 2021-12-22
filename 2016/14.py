from Utils import *

# Part 1

inp = "jlmsuwbz"
results = dict()


def hash2016(s):
    h = hashlib.md5(s.encode('utf-8')).hexdigest()
    for i in range(2016):
        h = hashlib.md5(h.encode('utf-8')).hexdigest()

    return h


hashmap = dict()

i = 0
keys = 0
while True:
    s = inp + str(i)
    if i not in hashmap:
        hashmap[i] = hash2016(s)

    h = hashmap[i]

    if r := re.search(r'(.)\1\1', h):
        pat = r[0][1] * 5

        for j in range(i + 1, i + 1001):
            s = inp + str(j)

            if j not in hashmap:
                hashmap[j] = hash2016(s)

            h2 = hashmap[j]

            if z := re.search(pat, h2):
                keys += 1
                print(keys, i)
                break
    i += 1

# 8:23 (8th)

# Part 2

# 12:49 (3rd)


