from Utils import *

# Part 1

inp = rdl("13.txt")


class Cart:
    def __init__(self, facing, x, y):
        self.facing = facing
        self.x = x
        self.y = y
        self.intersections = 0

    def turn(self, direc):
        if direc == 'L':
            if self.facing == 'U':
                self.facing = 'L'
            elif self.facing == 'L':
                self.facing = 'D'
            elif self.facing == 'D':
                self.facing = 'R'
            elif self.facing == 'R':
                self.facing = 'U'
        elif direc == 'R':
            if self.facing == 'U':
                self.facing = 'R'
            elif self.facing == 'L':
                self.facing = 'U'
            elif self.facing == 'D':
                self.facing = 'L'
            elif self.facing == 'R':
                self.facing = 'D'

    def move(self, grid):
        # Change direction if necessary
        if self.facing == 'U':
            if grid[(self.x, self.y)] == '/':
                self.facing = 'R'
            elif grid[(self.x, self.y)] == '\\':
                self.facing = 'L'
            elif grid[(self.x, self.y)] == '+':
                self.intersection()
        elif self.facing == 'D':
            if grid[(self.x, self.y)] == '/':
                self.facing = 'L'
            elif grid[(self.x, self.y)] == '\\':
                self.facing = 'R'
            elif grid[(self.x, self.y)] == '+':
                self.intersection()
        elif self.facing == 'L':
            if grid[(self.x, self.y)] == '/':
                self.facing = 'D'
            elif grid[(self.x, self.y)] == '\\':
                self.facing = 'U'
            elif grid[(self.x, self.y)] == '+':
                self.intersection()
        elif self.facing == 'R':
            if grid[(self.x, self.y)] == '/':
                self.facing = 'U'
            elif grid[(self.x, self.y)] == '\\':
                self.facing = 'D'
            elif grid[(self.x, self.y)] == '+':
                self.intersection()

        # Move
        if self.facing == 'U':
            self.y -= 1
        elif self.facing == 'D':
            self.y += 1
        elif self.facing == 'L':
            self.x -= 1
        elif self.facing == 'R':
            self.x += 1

    def intersection(self):
        self.intersections += 1
        if (self.intersections % 3) == 1:
            self.turn('L')
        elif (self.intersections % 3) == 2:
            pass
        elif (self.intersections % 3) == 0:
            self.turn('R')

    def __str__(self):
        return f'({self.x}, {self.y}): {self.facing}'

    def __repr__(self):
        return f'({self.x}, {self.y}): {self.facing}'

# Initialise grid + carts


grid = dict()
carts = list()

for y in range(len(inp)):
    for x in range(len(inp[1])):
        if x >= len(inp[y]):
            tile = ' '
        else:
            tile = inp[y][x]

        grid[x, y] = tile

        if tile in ['^', 'v', '<', '>']:
            if tile == '^':
                facing = 'U'
            elif tile == 'v':
                facing = 'D'
            elif tile== '<':
                facing = 'L'
            else:
                facing = 'R'
            carts.append(Cart(facing, x, y))
            grid[x, y] = '|'


# Move carts


while True:
    crashed = []
    carts = sorted(carts, key=lambda cart: (cart.y, cart.x))
    for i in range(len(carts)):
        if i not in crashed:
            carts[i].move(grid)

            # Check collisions
            for j in range(len(carts)):
                if i != j and j not in crashed:
                    if carts[i].x == carts[j].x and carts[i].y == carts[j].y:
                        crashed.append(i)
                        crashed.append(j)

    crashed.sort(reverse=True)
    for c in crashed:
        del carts[c]

    if len(carts) == 1:
        print(carts)
        exit(1)

# 33:48 (> 100th)

# 37:57 (66th)

