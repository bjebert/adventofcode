from Utils import *

# Part 1

# inp = rdl("20.txt")
#
# splits = [x.split('-') for x in inp]
#
# lo = [int(x[0]) for x in splits]
# hi = [int(x[1]) for x in splits]
#
# print(lo)
# print(hi)
#
# # Lowest value not contained between ranges
#
# lo_sorted = list(lo)
# lo_sorted.sort()
#
# print(lo_sorted)
# sort_idx = []
#
# for i in lo_sorted:
#     sort_idx += [x for x, y in enumerate(lo) if y == i]
#
# print(sort_idx)
#
# lowest = 0
# for i in sort_idx:
#     if lo[i] <= lowest:
#         lowest = hi[i] + 1
#
# print(lowest)

# 5:51 (37th)

inp = rdl("20.txt")

# inp = ['5-8', '0-2', '4-7']

splits = [x.split('-') for x in inp]

lo = [int(x[0]) for x in splits]
hi = [int(x[1]) for x in splits]

lo_sorted = list(lo)
lo_sorted.sort()
sort_idx = []

for i in lo_sorted:
    sort_idx += [x for x, y in enumerate(lo) if y == i]

lowest = 0
count = 0
for i in range(len(sort_idx)):
    if lo[sort_idx[i]] > lowest:
        count += lo[sort_idx[i]] - lowest
    lowest = max(lowest, hi[sort_idx[i]] + 1)

count += 4294967295 + 1 - lowest

print(count)

# 23:40 (> 100th)