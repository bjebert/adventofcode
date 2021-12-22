from Read import *

inp = rdl('8.txt')

# Part 1

lengths = [len(x) for x in inp]
escape2 = [len(eval(x)) for x in inp]
print(sum(lengths) - sum(escape2))

# 44:18 (> 100th)

print(sum([2+x.count('"')+x.count('\\') for x in inp]))

# 47:25 (> 100th)
