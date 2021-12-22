from Utils import *

# Part 1

inp = rdl("22.txt")[2:]

# inp = """Filesystem            Size  Used  Avail  Use%
# /dev/grid/node-x0-y0   10T    8T     2T   80%
# /dev/grid/node-x0-y1   11T    6T     5T   54%
# /dev/grid/node-x0-y2   32T   28T     4T   87%
# /dev/grid/node-x1-y0    9T    7T     2T   77%
# /dev/grid/node-x1-y1    8T    0T     8T    0%
# /dev/grid/node-x1-y2   11T    7T     4T   63%
# /dev/grid/node-x2-y0   10T    6T     4T   60%
# /dev/grid/node-x2-y1    9T    8T     1T   88%
# /dev/grid/node-x2-y2    9T    6T     3T   66%""".split('\n')[1:]


nodes_data = [x.split(' ')[0] for x in inp]
node_loc = [n.split('-')[1:] for n in nodes_data]

coords = [(int(x[0][1:]), int(x[1][1:])) for x in node_loc]

used = [int(x.split()[2][:-1]) for x in inp]
avail = [int(x.split()[3][:-1]) for x in inp]

count = 0
for a in range(len(inp)):
    for b in range(len(inp)):
        if a != b:
            if used[a] > 0 and used[a] <= avail[b]:
                count += 1

print(count)

# 5:40 (10th)

nodes = dict()
for i in range(len(coords)):
    nodes[coords[i]] = [used[i], avail[i]]


# def get_moves(nodes, x_size, y_size):
#     moves = []
#     for n in nodes:
#         x1, y1 = n
#
#         for x2 in range(x1 - 1, x1 + 2):
#             for y2 in range(y1 - 1, y1 + 2):
#                 if 0 <= x2 <= x_size:
#                     if 0 <= y2 <= y_size:
#                         if not (x1 == x2 and y1 == y2):
#                             if abs(x2 - x1) + abs(y2 - y1) == 1:
#                                 if nodes[(x2, y2)][1] >= nodes[n][0] > 0:
#                                     # Move data from x1, y1 to x2, y2
#                                     new_nodes = copy.deepcopy(nodes)
#                                     move_size = new_nodes[n][0]
#
#                                     avail_change = move_size
#                                     if move_size % 1 != 0:
#                                         avail_change += 0.5
#
#                                     new_nodes[n][0] = 0
#                                     new_nodes[n][1] += avail_change
#
#                                     new_nodes[(x2, y2)][0] += move_size
#                                     new_nodes[(x2, y2)][1] -= avail_change
#
#                                     moves.append(new_nodes)
#     return moves
#
#
# def bfs():
#     x_size = max([n[0] for n in nodes])
#     y_size = max([n[1] for n in nodes])
#     nodes[x_size, 0][0] -= 0.5
#     goal = nodes[x_size, 0][0]
#
#     # Get all available moves
#     queue = [[nodes]]
#     visited = set()
#     visited.add(json.dumps(list(nodes.items())))
#
#     i = 0
#     while queue:
#         path = queue.pop(0)
#
#         curr_nodes = path[-1]
#
#         if curr_nodes[(0, 0)][0] == goal:
#             return len(path) - 1
#
#         moves = get_moves(curr_nodes, x_size, y_size)
#
#         for move in moves:
#             if json.dumps(list(move.items())) in visited:
#                 continue
#
#             visited.add(json.dumps(list(move.items())))
#             new_path = list(path)
#             new_path.append(move)
#             queue.append(new_path)
#         i += 1
#         if i % 100 == 0:
#             print(i, len(path))
#
#     return 'No path found!'
#
#
# print(bfs())

for y in range(31):
    z = ''
    for x in range(32):
        if nodes[(x, y)][0] == 0:
            z += '0 '
        elif nodes[(x, y)][0] < 150:
            z += '. '
        else:
            z += '# '
    print(z)

# 1:32:20 (>100th)
