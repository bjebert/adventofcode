# A string-based approach to Day 18

from Utils import *

inp = rdl("18.txt")


def explode(snail):
    i = 0
    depth = 0
    while i < len(snail):
        c = snail[i]
        if c == '[':
            depth += 1
        elif c == ']':
            depth -= 1

        if depth >= 5:
            l, r = [int(x) for x in "".join(snail[i+1:]).split(']')[0].split('[')[-1].split(', ')]
            end = i + 1 + [x for x, y in enumerate(snail[i+1:]) if y == ']'][0]

            # Increase left-value
            for j in range(i, 0, -1):
                if snail[j].isnumeric():
                    snail[j] = str(int(snail[j]) + l)
                    break

            # Increase right-value
            for j in range(end, len(snail)):
                if snail[j].isnumeric():
                    snail[j] = str(int(snail[j]) + r)
                    break

            # Set value to 0
            return snail[:i] + ['0'] + snail[end+1:], True

        i += 1
    return snail, False


def split(snail):
    i = 0
    while i < len(snail):
        if snail[i].isnumeric() and int(snail[i]) >= 10:
            num = int(snail[i])
            sep = [math.floor(num / 2), math.ceil(num / 2)]

            return snail[:i] + ['[', str(sep[0]), ',', ' ', str(sep[1]), ']'] + snail[i+1:], True
        i += 1

    return snail, False


def add(l1, l2):
    snail = list(str([eval(l1), eval(l2)]))
    is_explode, is_split = False, False

    while True:
        snail, is_explode = explode(snail)
        if not is_explode:
            snail, is_split = split(snail)

        if not is_explode and not is_split:
            return "".join(snail)


def magnitude(snail):
    if type(snail) == int:
        return snail
    else:
        return 3 * magnitude(snail[0]) + 2 * magnitude(snail[1])


result = magnitude(eval(reduce(add, inp)))
print(result)

print(max([magnitude(eval(add(x, y))) for x, y in permutations(inp, 2)]))