from Utils import *

# Part 1

inp = rdl("8.txt")

c = 0

disp = {0: 'abcefg', 1: 'cf', 2: 'acdeg', 3: 'acdfg', 4: 'bcdf', 5: 'abdfg', 6: 'abdefg', 7: 'acf', 8: 'abcdefg',
        9: 'abcdfg'}




def remove_singles_doubles(sigmap):
    removed = False
    # Remove single letters
    for i in sigmap:
        if len(sigmap[i]) == 1:
            for k in sigmap:
                if k != i and sigmap[i][0] in sigmap[k]:
                    sigmap[k].remove(sigmap[i][0])
                    removed = True

    # If we have two letters shared between two chars (i.e 0 = 'cf', 2 = 'cf', then remove from others)
    for i in sigmap:
        for j in sigmap:
            if i != j and sigmap[i] == sigmap[j] and len(sigmap[i]) == 2:
                for k in sigmap:
                    if k != i and k != j:
                        for c in sigmap[i]:
                            if c in sigmap[k]:
                                sigmap[k].remove(c)
                                removed = True

    return removed

def make_outs(sigmap):
    n = ''
    for o in outs:
        s = "".join(sorted([sigmap[x][0] for x in o]))
        n += str([x for x in disp if disp[x] == s][0])

    return int(n)

c = 0
for line in inp:
    l, r = line.split(' | ')

    ins = l.split(' ')
    outs = r.split(' ')

    nums = list(range(0, 10))

    sigmap = dict()
    for letter in 'abcdefg':
        sigmap[letter] = list('abcdefg')

    while any([len(x) != 1 for x in sigmap.values()]):
        removed = False
        for x in ins:
            if sum([len(disp[n]) == len(x) for n in nums]) == 1:
                for n in nums:
                    if len(disp[n]) == len(x):
                        for i in x:
                            sigmap[i] = [x for x in sigmap[i] if x in disp[n]]
                            removed = True
                        nums.remove(n)
                        continue

        removed = remove_singles_doubles(sigmap)

        # Are there characters that appear in all 5-segment numbers?
        for i in sigmap:
            for N in [5, 6]:
                if all([i in x for x in ins if len(x) == N]):
                    # Which segments appear in all outputs?

                    possible = []
                    for j in sigmap:
                        if all([j in disp[x] for x in disp if len(disp[x]) == N]):
                            possible.append(j)

                    for k in sigmap[i]:
                        if k not in possible:
                            sigmap[i].remove(k)

    out = make_outs(sigmap)
    c += out

print(c)


