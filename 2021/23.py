cups = [7, 8, 4, 2, 3, 5, 9, 1, 6]
#cups = [3, 8, 9, 1, 2, 5, 4, 6, 7]

cups = cups + [x for x in range(10, 1000001)]

cup_next = dict()
for i in range(len(cups)):
    cup_next[cups[i]] = cups[(i+1) % len(cups)]

cup_prev = dict()
for i in range(len(cups)):
    cup_prev[cups[i]] = cups[(i-1) % len(cups)]


import time
start_time = time.time()

current = cups[0]
for i in range(10000000):
    pickup_1 = cup_next[current]
    pickup_2 = cup_next[pickup_1]
    pickup_3 = cup_next[pickup_2]

    destination_order = [(x - 1) % len(cups) + 1 for x in range(current - 1, current - 5, -1)]

    dest = 0
    for d in destination_order:
        if d == pickup_1 or d == pickup_2 or d == pickup_3:
            next
        else:
            dest = d
            break

    old_pickup_1_prev = cup_prev[pickup_1]
    
    cup_prev[cup_next[dest]] = pickup_3
    cup_prev[cup_next[pickup_3]] = cup_prev[pickup_1]
    cup_prev[pickup_1] = dest

    cup_next[old_pickup_1_prev] = cup_next[pickup_3]
    cup_next[pickup_3] = cup_next[dest]
    cup_next[dest] = pickup_1

    current = cup_next[current]


#print(cup_next)
print(cup_next[1] * cup_next[cup_next[1]])
print("--- %s seconds ---" % (time.time() - start_time))

    
