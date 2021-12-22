from Utils import *

# Part 1

inp = "11100010111110100"
disk_len = 35651584


def fill_disk(a, desired):
    while len(a) < desired:
        b = a[-1::-1]
        b = "".join([str(int(x == '0')) for x in b])
        a = a + '0' + b

        print(len(a))

    return a[:disk_len]


data = fill_disk(inp, disk_len)
checksum = ''

while len(data) % 2 == 0:
    print(len(data))
    checksum = ''
    for i in range(0, len(data), 2):
        pair = data[i:i+2]

        if pair[0] == pair[1]:
            checksum += '1'
        else:
            checksum += '0'

    data = checksum

print(data)

# 10:12 (87th)

# Part 2

# 11:16 (68th)