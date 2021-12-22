from Read import *
import re
import math

# Part 1

inp = rdl("15.txt")
#
# inp = """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
# Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3""".split('\n')

sweets = dict()

for line in inp:
    ss = line.split(' ')
    sweets[ss[0][:-1]] = {'capacity': int(ss[2][:-1]),
                          'durability': int(ss[4][:-1]),
                          'flavor': int(ss[6][:-1]),
                          'texture': int(ss[8][:-1]),
                          'calories': int(ss[-1])}

max_prod = 0
for i in range(101):
    for j in range(101 - i):
        for k in range(101 - i - j):
            l = 100 - i - j - k

            prod = 1
            for x in ['capacity', 'durability', 'flavor', 'texture']:
                score = sweets['Sprinkles'][x] * i + \
                        sweets['Butterscotch'][x] * j + \
                        sweets['Chocolate'][x] * k + \
                        sweets['Candy'][x] * l

                if score < 0:
                    prod = 0

                prod *= score

            cals = sweets['Sprinkles']['calories'] * i + \
                   sweets['Butterscotch']['calories'] * j + \
                   sweets['Chocolate']['calories'] * k + \
                   sweets['Candy']['calories'] * l

            if prod > max_prod and cals == 500:
                print(prod)
                max_prod = prod

# 10:52 (12th)

# Part 2

# 12:42 (13th)
