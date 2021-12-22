from Utils import *

# Part 1

inp = "wenycdww"
# inp = "flqrgnkx"

grid = []

def knot_hash(s):
    inp_conv = [ord(x) for x in s] + [17, 31, 73, 47, 23]
    list_size = 256

    pos = 0
    skip = 0

    lst = [i for i in range(list_size)]
    for _ in range(64):
        for i in inp_conv:
            if i > 1:
                sub = lst[pos:(pos + i)]
                idx = list(range(pos, pos + i))[:len(sub)]

                if pos + i > len(lst):
                    sub += lst[:(pos + i) % len(lst)]
                    idx += list(range((pos + i) % len(lst)))

                rev = sub[::-1]
                for x in range(len(idx)):
                    lst[idx[x]] = rev[x]

            pos = (pos + i + skip) % len(lst)
            skip += 1

    bits = []
    for j in range(0, 256, 16):
        bits.append(reduce(lambda a, b: a ^ b, lst[j:(j + 16)]))

    unpadded = [str(hex(x)).split('x')[1] for x in bits]
    padded = ['0' + x if len(x) == 1 else x for x in unpadded]

    return "".join(padded)


for i in range(128):
    row_hash = knot_hash(inp + '-' + str(i))
    grid.append([])

    for c in row_hash:
        scale = 16  ## equals to hexadecimal
        num_of_bits = 4
        bits = bin(int(c, scale))[2:].zfill(num_of_bits)

        for b in bits:
            grid[i].append(int(b))

print(sum([sum(x) for x in grid]))

# 6:22 (36th)

# grid = """11010100
# 01010101
# 00001010
# 10101101
# 01101000
# 11001001
# 01000100
# 11010110""".split('\n')

# grid = [[int(y) for y in list(x.strip())] for x in grid]

# print(grid)

visited = set()
regions = 0

for x in range(len(grid)):
    for y in range(len(grid)):
        if (x, y) in visited:
            continue

        if grid[x][y] == 1:
            regions += 1
            queue = [(x, y)]
            visited.add((x, y))

            while queue:
                curr = queue.pop()

                # Get adjacents
                for coord in [(curr[0] - 1, curr[1]), (curr[0] + 1, curr[1]), (curr[0], curr[1] - 1), (curr[0], curr[1] + 1)]:
                    if 0 <= coord[0] < len(grid) and 0 <= coord[1] < len(grid):
                        if grid[coord[0]][coord[1]] == 1 and coord not in visited:
                            visited.add(coord)
                            queue.append(coord)

print(regions)

# 19:52 (51st)

