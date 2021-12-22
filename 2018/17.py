from Utils import *

# Part 1

inp = rdl("17.txt")

# inp = """x=495, y=2..7
# y=7, x=495..501
# x=501, y=3..7
# x=498, y=2..4
# x=506, y=1..2
# x=498, y=10..13
# x=504, y=10..13
# y=13, x=498..504""".split('\n')

grid = defaultdict(lambda: '.')

for line in inp:
    l, r = line.split(', ')

    start = int(r.split('..')[0].split('=')[1])
    end = int(r.split('..')[1])

    lv = int(l.split('=')[1])
    for i in range(start, end+1):
        if l[0] == 'x':
            grid[lv, i] = '#'
        else:
            grid[i, lv] = '#'

min_y = min([k[1] for k in grid])
max_y = max([k[1] for k in grid])

last = -1
springs = [(500, 0)]
while sum([x in ['~', '|'] for x in grid.values()]) != last:
    last = sum([x in ['~', '|'] for x in grid.values()])

    # Create water from spring
    for spring in springs:
        x, y = spring
        bottom = False

        while grid[(x, y+1)] not in ('#', '~'):  # Fall down
            if y+1 > max_y:
                bottom = True
                springs.remove(spring)
                break
            else:
                grid[(x, y + 1)] = '|'
                y += 1

        if bottom:
            break

        # Seek left and right; if it can fall going either direction, then mark it as sand, otherwise settle
        can_fall = False
        seek = set()

        seek.add((x, y))
        seek_start = x, y

        # Seek left
        while grid[(x, y+1)] in ('#', '~') and grid[(x-1, y)] not in ('#', '~'):
            x -= 1
            seek.add((x, y))

            if grid[(x, y+1)] not in ('#', '~'):
                can_fall = True

                if (x, y) not in springs:
                    springs.append((x, y))

        x, y = seek_start

        # Seek right
        while grid[(x, y+1)] in ('#', '~') and grid[(x+1, y)] not in ('#', '~'):
            x += 1
            seek.add((x, y))

            if grid[(x, y+1)] not in ('#', '~'):
                can_fall = True

                if (x, y) not in springs:
                    springs.append((x, y))

        for s in seek:
            grid[s] = '|' if can_fall else '~'

        if not len(seek):
            break

c = 0
for k in grid:
    if min_y <= k[1] <= max_y:
        if grid[k] == '~':
            c += 1

print(c)

# slight bug in part 1 - had to manually add twelve squares which weren't reached

# > 24h



