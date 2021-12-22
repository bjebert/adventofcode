from Utils import *

# Part 1


def floors2hash(floors, curr):
    simplify_floors(floors)
    return json.dumps((floors, curr))


def get_end_node(floors, start_floor, end_floor, items_to_move):
    result = copy.deepcopy(floors)

    for item in items_to_move:
        result[start_floor].remove(item)
        result[end_floor].append(item)

        result[start_floor].sort()
        result[end_floor].sort()

    return end_floor, result


def is_safe(floors):
    for f in floors:
        chips = [x[0] for x in floors[f] if x[1] == "M"]
        rtgs = [x[0] for x in floors[f] if x[1] == "G"]

        unmatched_chips = [c for c in chips if c not in rtgs]
        if len(unmatched_chips) and len(rtgs):
            return False

    return True


def get_available_nodes(floors, curr):
    dirs = []
    if curr + 1 <= 4:
        dirs.append(curr + 1)
    if curr - 1 >= 1:
        dirs.append(curr - 1)

    curr_items = floors[curr]

    # Can move max two items
    item_combinations = []
    for i in [1, 2]:
        item_combinations += list(combinations(curr_items, i))

    nodes = []
    for end_floor in dirs:
        for items_to_move in item_combinations:
            node = get_end_node(floors, curr, end_floor, items_to_move)
            if is_safe(node[1]):
                nodes.append(node)

    return nodes


def simplify_floors(floors):
    elements = dict()
    i = 0

    for f in floors:
        elem = unique([item[0] for item in floors[f]])
        elem.sort()

        for e in elem:
            if e not in elements:
                elements[e] = str(i)
                i += 1

    for f in floors:
        floors[f] = [elements[x[0]] + x[1] for x in floors[f]]
        floors[f].sort()


def bfs(floors):
    queue = [[(1, floors)]]
    visited = set()
    visited.add(floors2hash(floors, 1))
    i = 0

    while queue:
        i += 1
        path = queue.pop(0)

        neighbours = get_available_nodes(path[-1][1], path[-1][0])

        if i % 2000 == 0:
            print(i, len(path))

        for neighbour in neighbours:
            if sum([len(neighbour[1][x]) for x in range(1, 4)]) == 0:  # End
                return path

            ng_hash = floors2hash(neighbour[1], neighbour[0])

            if ng_hash not in visited:
                visited.add(ng_hash)
                new_path = list(path)
                new_path.append(neighbour)
                queue.append(new_path)


floors = {1: ['SG', 'SM', 'PG', 'PM', 'EG', 'EM', 'DG', 'DM'],
          2: ['TG', 'RG', 'RM', 'CG', 'CM'],
          3: ['TM'],
          4: []}

# floors = {1: ['HM', 'LM'],
#           2: ['HG'],
#           3: ['LG'],
#           4: []}


result = bfs(floors)

for r in result:
    print(r)

print(len(result))

# >24h (>100th)

# Part 2

# >24h (>100th)


