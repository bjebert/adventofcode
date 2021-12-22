from Utils import *

# Part 1

inp = rdl("13.txt")

firewall = dict()
scanner = dict()
direct = dict()

# inp = """0: 3
# 1: 2
# 4: 4
# 6: 4""".split('\n')

for line in inp:
    ls = line.split(': ')
    firewall[int(ls[0])] = int(ls[1])
    scanner[int(ls[0])] = 0
    direct[int(ls[0])] = 1

pos = 0
severity = 0

while pos <= max(list(firewall.keys())):
    if pos in scanner:
        if scanner[pos] == 0:
            severity += pos * firewall[pos]

    pos += 1

    for k in scanner:
        if scanner[k] == (firewall[k] - 1) and direct[k] == 1:
            direct[k] = -1
        elif scanner[k] == 0 and direct[k] == -1:
            direct[k] = 1

        scanner[k] += direct[k]

print(severity)

# 8:57 (T-59th)

# Part 2


def good(t):
    for k in firewall:
        if t + k in range(0, t+100, 2 * (firewall[k] - 1)):
            return False
    return True


t = 0
while True:
    if good(t):
        print(t)
        break
    t += 1

# 24:01 (> 100th)

