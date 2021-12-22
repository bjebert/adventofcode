from Utils import *

# Part 1

inp = 3014387
ne = 3014387


def linked(n):
    elves = dict(
        zip([x for x in range(n)], [[1, (x - 1) % n, (x + 1) % n] for x in range(n)]))

    curr = 0
    while sum([elves[e][0] == 0 for e in elves]) != n - 1:
        if elves[curr][0] > 0:
            elves[curr][0] += elves[elves[curr][2]][0]
            elves[elves[curr][2]][0] = 0

            out = elves[curr][2]

            elves[elves[out][2]][1] = elves[out][1]  # Reassign left
            elves[elves[out][1]][2] = elves[out][2]  # Reassign right

        curr = (curr + 1) % n

    return [e + 1 for e in elves if elves[e][0] > 0][0]


def get_elf(n):
    fl = math.floor(math.log(n, 2))
    base = int(math.pow(2, fl))

    return (int(n) - base + 1) * 2 - 1


# print(get_elf(inp))

# 19:11 (> 100th)


def get_opposite(n):
    opps = []
    for x in range(1, n + 1):
        opp = (x + n / 2) % n
        if abs(opp % 1 - 0) > 0.01:
            opp = int(opp-0.5)

        if opp == 0:
            opp = n

        opps.append(int(opp))
    return opps


def circle(n):
    elves = dict()
    opposites = get_opposite(n)

    for x in range(n):
        elves[x+1] = [1, opposites[x]]

    curr = 1

    while len(elves) > 1:
        elves[curr][0] += elves[elves[curr][1]][0]
        elves[elves[curr][1]][0] = 0

        # Remove outed elf and reshuffle circle
        out = elves[curr][1]

        del elves[out]
        opposites = get_opposite(len(elves))

        i = 0
        k = list(elves.keys())
        for e in k:
            elves[e][1] = k[opposites[i] - 1]
            i += 1

        curr = k[([x for x, y in enumerate(k) if y == curr][0] + 1) % len(k)]

    return list(elves.keys())[0]


print([circle(x) for x in range(2, 50)])

z = [1, 3]
while len(z) < inp:
    z += list(range(1, max(z) + 1))
    z += list(range(max(z)+2, max(z)+2 + max(z)*2, 2))

print([z[x-2] for x in range(2, 50)])

print(z[inp-2])

# 1:04:56 (76th)
