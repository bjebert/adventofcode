from Utils import *

# Part 1

inp = rdl("14.txt")

tmp = inp[0]
pair = dict()

for line in inp[2:]:
    l, _, r = line.split()
    pair[l] = r

counts = defaultdict(int)
for k in pair:
    counts[k] = tmp.count(k)

for _ in range(40):
    nc = copy.deepcopy(counts)
    for k in counts:
        nc[k[0] + pair[k]] += counts[k]
        nc[pair[k] + k[1]] += counts[k]
        nc[k] -= counts[k]

    counts = copy.deepcopy(nc)

    single = defaultdict(int)
    for k in counts:
        single[k[0]] += counts[k]

    single[tmp[-1]] += 1
    print(_+1, max(single.values()) - min(single.values()))

