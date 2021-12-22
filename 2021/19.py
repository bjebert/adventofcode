from Utils import *

inp = rdl("19.txt")
# inp = rdl("19a.txt")

scan = defaultdict(list)

# --- Parse input ---

for line in inp:
    if '---' in line:
        curr = int(line[12:14].strip())
    else:
        if len(line) > 5:
            scan[curr].append(tuple([int(x) for x in line.split(',')]))

# can be facing positive or negative x,y,z and can consider any four directions up (24 different orietnations)

scanner_locations = {0: ((0, 0, 0), 0, 1, 2, 1, 1, 1)}
beacon_grid = set(scan[0])  # Relative to scanner 0 at (0, 0, 0)

# Try each beacon in all 24 directions from scanner 1, and see if we get 12 overlapping:


def overlapping(i):
    for beacon in beacon_grid:
        for coord in scan[i]:
            for x in [0, 1, 2]:
                for y in [0, 1, 2]:
                    for z in [0, 1, 2]:
                        if x != y and x != z and y != z:
                            for s1 in [1, -1]:
                                for s2 in [1, -1]:
                                    for s3 in [1, -1]:
                                        loc = beacon[0] + s1 * coord[x], beacon[1] + s2 * coord[y], beacon[2] + s3 * coord[z]

                                        found = 0
                                        for k in range(len(scan[i])):  # Using same transformation, see if we can find others relative
                                            c2 = scan[i][k]
                                            new = loc[0] - s1 * c2[x], loc[1] - s2 * c2[y], loc[2] - s3 * c2[z]
                                            if new in beacon_grid:
                                                found += 1

                                            if k == 14 and found <= 1:
                                                break

                                        if found >= 12:
                                            if i not in scanner_locations:
                                                scanner_locations[i] = (loc, x, y, z, s1, s2, s3)
                                                return True

    return False


update = True
while update:
    update = False
    for i in range(max(scan.keys()) + 1):
        if i not in scanner_locations:
            if overlapping(i):
                print(i)
                update = True

                # Change all beacons to be relative to scanner 0 (0, 0, 0)
                for k in range(len(scan[i])):
                    beacon = scan[i][k]
                    loc = scanner_locations[i][0]
                    x, y, z, s1, s2, s3 = scanner_locations[i][1:]
                    new_beacon = loc[0] - s1 * beacon[x], loc[1] - s2 * beacon[y], loc[2] - s3 * beacon[z]
                    scan[i][k] = new_beacon

                    # Add new beacons to beacon grid
                    beacon_grid.add(new_beacon)


print(scanner_locations)
print(len(beacon_grid))


def manhattan(loc1, loc2):
    return abs(loc1[0] - loc2[0]) + abs(loc1[1] - loc2[1]) + abs(loc1[2] - loc2[2])


locs = [scanner_locations[x][0] for x in scanner_locations]
print(max([manhattan(x[0], x[1]) for x in permutations(locs, 2)]))
