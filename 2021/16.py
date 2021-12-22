from Utils import *

# Part 1

inp = rd("16.txt")


def parse_input(inp):
    return "".join([str(bin(int(c, 16))[2:].zfill(4)) for c in inp])


def parse(bits, n_sub=math.inf):
    packets = []

    i = 0
    while i < len(bits) and len(packets) < n_sub:
        if all([x == '0' for x in bits[i:]]):
            break

        version, type_id = int(bits[i:i+3], 2), int(bits[i+3:i+6], 2)

        if type_id == 4:
            val_str = ''
            i += 6
            while True:
                group = bits[(i+1):(i+5)]
                while len(group) < 4:
                    group += '0'
                val_str += group

                i += 5
                if bits[i-5] == '0':
                    break
            packets.append((version, type_id, int(val_str, 2)))
        else:
            if bits[i+6] == '0':
                total_length = int(bits[i+7:i+22], 2)
                sub_packets, _ = parse(bits[i+22:i+22+total_length])
                i += 22+total_length
            else:
                sub_packets, j = parse(bits[i+18:], int(bits[i+7:i+18], 2))
                i += 18+j

            packets.append((version, type_id, sub_packets))

    return packets, i


packets, _ = parse(parse_input(inp))


def get_version_sum(packets, v=0):
    if type(packets[2]) == int:
        return packets[0]

    version_sum = packets[0]
    for p in packets[2]:
        version_sum += get_version_sum(p, v)

    return version_sum


print(get_version_sum(packets[0]))  # Part 1


def calculate(packets):
    type_id = packets[1]

    if type_id == 0:
        return sum([calculate(p) for p in packets[2]])
    elif type_id == 1:
        return reduce(lambda x, y: x * y, [calculate(p) for p in packets[2]])
    elif type_id == 2:
        return min([calculate(p) for p in packets[2]])
    elif type_id == 3:
        return max([calculate(p) for p in packets[2]])
    elif type_id == 4:
        return packets[2]
    elif type_id == 5:
        return 1 if calculate(packets[2][0]) > calculate(packets[2][1]) else 0
    elif type_id == 6:
        return 1 if calculate(packets[2][0]) < calculate(packets[2][1]) else 0
    elif type_id == 7:
        return 1 if calculate(packets[2][0]) == calculate(packets[2][1]) else 0


print(calculate(packets[0]))  # Part 2
