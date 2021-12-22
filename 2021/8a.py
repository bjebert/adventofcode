from Utils import *

# Part 1

inp = rdl("8.txt")

disp = {0: 'abcefg', 1: 'cf', 2: 'acdeg', 3: 'acdfg', 4: 'bcdf', 5: 'abdfg', 6: 'abdefg', 7: 'acf', 8: 'abcdefg',
        9: 'abcdfg'}

# Part 1

c = 0
for line in inp:
    l, r = line.split(' | ')
    outs = r.split(' ')

    c += sum([len(x) in [2, 3, 4, 7] for x in outs])

print(c)

# Part 2

c = 0
for line in inp:
    l, r = line.split(' | ')
    ins = l.split(' ')
    outs = r.split(' ')

    n = ''
    for p in permutations('abcdefg'):
        pmap = dict(zip(['a', 'b', 'c', 'd', 'e', 'f', 'g'], p))
        if all(["".join(sorted([pmap[x] for x in o])) in disp.values() for o in outs + ins]):
            for o in outs:
                for d in disp:
                    if disp[d] == "".join(sorted([pmap[x] for x in o])):
                        n += str(d)
                        break
            c += int(n)
            break

print(c)