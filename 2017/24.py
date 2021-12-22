from Utils import *

# Part 1

inp = rdl("24.txt")

# inp = """0/2
# 2/2
# 2/3
# 3/4
# 3/5
# 0/1
# 10/1
# 9/10""".split('\n')

ports = dict()

i = 1
for line in inp:
    ls = line.split('/')
    ports[i] = (int(ls[0]), int(ls[1]))
    i += 1


def bridge(ports, curr):
    last = curr[-1][1]

    for p in ports:
        if ports[p][0] == last:
            new = list(curr)
            new.append(ports[p])

            new_ports = copy.deepcopy(ports)
            del new_ports[p]
            bridge(new_ports, new)
        elif ports[p][1] == last:
            new = list(curr)
            new.append((ports[p][1], ports[p][0]))

            new_ports = copy.deepcopy(ports)
            del new_ports[p]
            bridge(new_ports, new)

    score = sum([sum(x) for x in curr])
    length = len(curr)

    if length >= 32:
        print(score)

bridge(copy.deepcopy(ports), curr=[(0, 0)])

# 21:23 (> 100th)

# Part 2

# 23:47 (> 100th)
