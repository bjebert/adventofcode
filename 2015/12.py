from Read import *
import re

inp = eval(rdl("12.txt")[0])

# Part 1

raw = str(rdl("12.txt")[0])
x = re.findall(r'-?[0-9]+', raw)
print(sum([int(x) for x in x]))

# 7:08 (91st)

# Part 2


def dive(obj):
    if isinstance(obj, int):
        return obj

    if isinstance(obj, str):
        return 0

    if isinstance(obj, dict):
        for k in obj:
            if isinstance(obj[k], str) and obj[k] == 'red':
                return 0

    count = 0
    for k in obj:
        if isinstance(obj, dict):
            count += dive(obj[k])
        elif isinstance(obj, list):
            count += dive(k)

    return count


print(dive(inp))

# 31:20 (> 100th)
