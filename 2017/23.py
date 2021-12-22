from Utils import *

# Part 1

inp = rdl("23.txt")

reg = {'a': 1, 'b': 0, 'c': 0, 'd': 0, 'e': 0, 'f': 0, 'g': 0, 'h': 0}

i = 0
count = 0
# while i < len(inp):
#     if reg['g'] == reg['b'] and i == 13:
#         a = 1
#
#     ls = inp[i].split(' ')
#
#     if ls[2].strip('-').isnumeric():
#         val = int(ls[2])
#     else:
#         val = reg[ls[2]]
#
#     if ls[0] == 'set':
#         reg[ls[1]] = val
#     elif ls[0] == 'sub':
#         reg[ls[1]] -= val
#     elif ls[0] == 'mul':
#         reg[ls[1]] *= val
#         count += 1
#     elif ls[0] == 'jnz':
#         if ls[1].strip('-').isnumeric():
#             val1 = int(ls[1])
#         else:
#             val1 = reg[ls[1]]
#
#         if val1 != 0:
#             i += (val - 1)
#
#     i += 1

# 2:49 (T-12th)


def isprime(x):
    if x <= 2:
        return True

    for i in range(2, math.ceil(math.sqrt(x)) + 1):
        if x % i == 0:
            return False

    return True


print(sum([not isprime(x) for x in range(106500, 123501, 17)]))

# 44:20 (67th)
