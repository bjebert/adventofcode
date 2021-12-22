from Utils import *

# Part 1

inp = rdl("7.txt")

def abba(line):
    bracks = re.findall('\\[[a-z]+\\]', line)
    for x in bracks:
        if abba(x[1:-1]):
            return False

    if len(bracks) == 0:
        if r := re.search(r'([a-z])([a-z])\2\1', line):
            if not r[0][0] == r[0][1]:
                return True
        else:
            return False

    pre = re.findall('[a-z]+\\[', line)
    post = re.findall('\\][a-z]+', line)

    for x in pre:
        chars = x[:-1]
        if r := re.search(r'([a-z])([a-z])\2\1', chars):
            if not r[0][0] == r[0][1]:
                return True

    for x in post:
        chars = x[1:]
        if r := re.search(r'([a-z])([a-z])\2\1', chars):
            if not r[0][0] == r[0][1]:
                return True

    return False

# print(abba('abba[mnop]qrst'))
# print(abba('abcd[bddb]xyyx'))
# print(abba('aaaa[qwer]tyui'))

# print(sum([abba(line) for line in inp]))

# 9:51 (T-49th)

def ssl(line):
    hypernet = [x[1:-1] for x in re.findall('\\[[a-z]+\\]', line)]
    supernet = [x.split(']')[-1] for x in line.split('[')]

    for sup in supernet:
        for r in re.finditer(r'([a-z])([a-z])\1', sup, overlapped=True):
            aba = r[0]
            if aba[0] == aba[1]:
                continue

            bab = aba[1] + aba[0] + aba[1]

            for hyp in hypernet:
                if s := re.search(bab, hyp):
                    return True

    return False


print(sum([ssl(line) for line in inp]))

# 39:58 (>100th)
