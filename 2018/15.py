from Utils import *

# Part 1

inp = rdl("15.txt")

# inp = """#########
# #G......#
# #.E.#...#
# #..##..G#
# #...##..#
# #...#...#
# #.G...G.#
# #.....G.#
# #########""".split('\n')

# inp = """#######
# #E..G.#
# #...#.#
# #.G.#G#
# #######""".split('\n')


def add_entities(grid):
    tmp_grid = copy.deepcopy(grid)
    for g in goblins:
        tmp_grid[(g.x, g.y)] = 'G'
    for e in elves:
        tmp_grid[(e.x, e.y)] = 'E'

    return tmp_grid


def find_path(start_x, start_y, target_x, target_y, grid, goblins, elves):
    q = [[(start_x, start_y)]]
    visited = set()
    visited.add((start_x, start_y))

    tmp_grid = add_entities(grid)

    while q:
        path = q.pop(0)
        x, y = path[-1]

        if (x, y) == (target_x, target_y):
            return path

        # order: up, left, right, down
        for x2, y2 in [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]:
            if (x2, y2) in tmp_grid and tmp_grid[(x2, y2)] == '.':
                if (x2, y2) not in visited:
                    visited.add((x2, y2))
                    new_path = list(path)
                    new_path.append((x2, y2))
                    q.append(new_path)


class Goblin:
    def __init__(self, x, y, hp, att):
        self.x = x
        self.y = y
        self.hp = hp
        self.att = att

    def find_targets(self, goblins, elves):
        targets = []
        for e in elves:
            for x, y in [(e.x, e.y - 1), (e.x - 1, e.y), (e.x + 1, e.y), (e.x, e.y + 1)]:
                if (x, y) in grid and grid[(x, y)] == '.':
                    targets.append((x, y))

        return targets

    def move(self, grid, goblins, elves):
        targets = self.find_targets(goblins, elves)

        # If already on a target, then don't move
        for x2, y2 in [(self.x, self.y - 1), (self.x - 1, self.y), (self.x + 1, self.y), (self.x, self.y + 1)]:
            # Is there a goblin on x2, y2?
            for g in elves:
                if g.x == x2 and g.y == y2:
                    return

        # Find shortest paths to a target
        paths = []
        for t in targets:
            path = find_path(self.x, self.y, t[0], t[1], grid, goblins, elves)
            if path:
                paths.append(path)

        # First shortest path
        if len(paths):
            min_len = min([len(x) for x in paths])
            if min_len > 1:
                shortest_paths = [x for x in paths if len(x) == min_len]
                dest = [x[-1] for x in shortest_paths]
                dest.sort(key=lambda x: (x[1], x[0]))

                shortest = [x for x in shortest_paths if x[-1] == dest[0]][0]

                # Move
                self.x = shortest[1][0]
                self.y = shortest[1][1]

    def attack(self, grid, goblins, elves):
        target = None
        min_hp = math.inf
        for x2, y2 in [(self.x, self.y - 1), (self.x - 1, self.y), (self.x + 1, self.y), (self.x, self.y + 1)]:
            # Is there a goblin on x2, y2?
            for g in elves:
                if g.x == x2 and g.y == y2:
                    if g.hp < min_hp:
                        min_hp = g.hp
                        target = x2, y2

        # Find elf at location t again
        if target:
            for g in elves:
                if g.x == target[0] and g.y == target[1]:
                    # Attack goblin
                    g.hp -= self.att

                    if g.hp <= 0:
                        return [g.x, g.y]
                    else:
                        return None

    def __repr__(self):
        return f'G({self.hp}): ({self.x}, {self.y})'


class Elf:
    def __init__(self, x, y, hp, att):
        self.x = x
        self.y = y
        self.hp = hp
        self.att = att

    def find_targets(self, goblins, elves):
        targets = []
        for g in goblins:
            for x, y in [(g.x, g.y - 1), (g.x - 1, g.y), (g.x + 1, g.y), (g.x, g.y + 1)]:
                if (x, y) in grid and grid[(x, y)] == '.':
                    targets.append((x, y))

        return targets

    def move(self, grid, goblins, elves):
        targets = self.find_targets(goblins, elves)

        # If already on a target, then don't move
        for x2, y2 in [(self.x, self.y - 1), (self.x - 1, self.y), (self.x + 1, self.y), (self.x, self.y + 1)]:
            # Is there a goblin on x2, y2?
            for g in goblins:
                if g.x == x2 and g.y == y2:
                    return

        # Find shortest paths to a target
        paths = []
        for t in targets:
            path = find_path(self.x, self.y, t[0], t[1], grid, goblins, elves)
            if path:
                paths.append(path)

        # First shortest path
        if len(paths):
            min_len = min([len(x) for x in paths])
            if min_len > 1:
                shortest_paths = [x for x in paths if len(x) == min_len]
                dest = [x[-1] for x in shortest_paths]
                dest.sort(key=lambda x: (x[1], x[0]))

                shortest = [x for x in shortest_paths if x[-1] == dest[0]][0]

                # Move
                self.x = shortest[1][0]
                self.y = shortest[1][1]

    def attack(self, grid, goblins, elves):

        # Adjacent target with fewest HP selected
        target = None
        min_hp = math.inf
        for x2, y2 in [(self.x, self.y - 1), (self.x - 1, self.y), (self.x + 1, self.y), (self.x, self.y + 1)]:
            # Is there a goblin on x2, y2?
            for g in goblins:
                if g.x == x2 and g.y == y2:
                    if g.hp < min_hp:
                        min_hp = g.hp
                        target = x2, y2

        # Find elf at location t again
        if target:
            for g in goblins:
                if g.x == target[0] and g.y == target[1]:
                    # Attack goblin
                    g.hp -= self.att

                    if g.hp <= 0:
                        return [g.x, g.y]
                    else:
                        return None

    def __repr__(self):
        return f'E({self.hp}): ({self.x}, {self.y})'


for elf_pow in range(13, 14):
    grid = dict()
    goblins = []
    elves = []
    for y in range(len(inp)):
        for x in range(len(inp[y])):
            tile = inp[y][x]
            grid[x, y] = tile

            if tile == 'G':
                grid[x, y] = '.'
                goblins.append(Goblin(x, y, 200, 3))
            elif tile == 'E':
                grid[x, y] = '.'
                elves.append(Elf(x, y, 200, elf_pow))

    entities = goblins + elves
    entities = sorted(entities, key=lambda e: (e.y, e.x))
    # print_grid(dict2grid(add_entities(grid)))

    j = 0
    rounds = 0
    while j < len(entities):
        if not len(goblins) or not len(elves):
            break

        entity = entities[j]

        entity.move(grid, goblins, elves)
        slain = entity.attack(grid, goblins, elves)

        if slain:
            # Remove slain (x, y) from entities

            dead = None
            for i in range(len(goblins)):
                if goblins[i].x == slain[0] and goblins[i].y == slain[1]:
                    dead = i
                    break

            if dead is not None:
                del goblins[i]

            dead = None
            for i in range(len(elves)):
                if elves[i].x == slain[0] and elves[i].y == slain[1]:
                    dead = i
                    break

            if dead is not None:
                del elves[i]
                break  # If any elves die, continue (part 2)

            dead = None
            for i in range(len(entities)):
                if entities[i].x == slain[0] and entities[i].y == slain[1]:
                    dead = i
                    break

            if dead is not None:
                del entities[i]

                if i < j:
                    j -= 1

        j += 1
        if j >= len(entities):
            j %= len(entities)
            entities = sorted(entities, key=lambda e: (e.y, e.x))
            rounds += 1

            print(f'Rounds: {rounds}, ElfPow: {elf_pow}, Goblins: {len(goblins)}, Elves: {len(elves)}, SumElfHP: {sum([e.hp for e in elves])}')
            # print_grid(dict2grid(add_entities(grid)))

    if len(elves) == 10:
        print(sum([g.hp for g in goblins]) * rounds, sum([g.hp for g in elves]) * rounds)

# 2:18:54 (> 100th)

# 2:44:30 (> 100th)