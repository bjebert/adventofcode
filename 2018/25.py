from Utils import *

# Part 1

inp = rdl("25.txt")

# inp = """0,0,0,0
#  3,0,0,0
#  0,3,0,0
#  0,0,3,0
#  0,0,0,3
#  0,0,0,6
#  9,0,0,0
# 12,0,0,0""".split('\n')

def can_reach(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1]) + abs(a[2] - b[2]) + abs(a[3] - b[3]) <= 3


stars = []
for line in inp:
    stars.append([int(x) for x in line.split(',')])


v = set()
c = 0

for star in stars:
    if tuple(star) not in v:
        v.add(tuple(star))
        c += 1
        q = [star]

        while q:
            curr = q.pop()

            for n in stars:
                if can_reach(curr, n):
                    if tuple(n) not in v:
                        v.add(tuple(n))
                        q.append(n)

print(c)

# 7:00 (38th) / 7:05 (29th)