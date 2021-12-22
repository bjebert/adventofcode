from Utils import *

# Part 1

serial = 9798
power = dict()

for x in range(1, 301):
    for y in range(1, 301):
        val = ((x+10) * y + serial) * (x+10)

        if len(str(val)) < 3:
            hund = 0
        else:
            hund = int(str(val)[-3])

        power[(x, y)] = hund - 5

# grid = dict2grid(power)
# print_grid(grid)

squares = dict()

max_power = 0
max_ij = None
for i in range(1, 301):
    for j in range(1, 301):
        for size in range(1, min([301-i, 301-j])+1):

            if (i, j, size-1) not in squares:
                sum_p = 0
                for k in range(size):
                    for l in range(size):
                        sum_p += power[i+k, j+l]
            else:
                sum_p = squares[(i, j, size-1)]

                for k in range(size):
                    sum_p += power[i+(size-1), j+k]
                for k in range(size-1):
                    sum_p += power[i+k, j+(size-1)]

            squares[(i, j, size)] = sum_p

            if sum_p > max_power:
                max_power = sum_p
                max_ij = i, j, size

                print(max_power)
                print(max_ij)

print(max_ij)

# 6:55 (77th)

# 20:31 (> 100th)


