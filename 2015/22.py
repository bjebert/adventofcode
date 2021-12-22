from Read import *
import re
import math
import itertools
import functools

# Part 1


def simulate_fight(strategy, hp, mana, boss_hp, boss_dmg):
    spent_mana = 0
    armor = [0, 0]
    poison = [0, 0]
    recharge = [0, 0]

    for turn in range(len(strategy)):
        # Our turn

        hp -= 1  # Part 2
        if hp <= 0:
            return False

        # Tick statuses
        if armor[1] <= 0:
            armor[0] = 0
            armor[1] = 0
        armor[1] -= 1

        if poison[1] <= 0:
            poison[0] = 0
            poison[1] = 0
        else:
            boss_hp -= poison[0]
        poison[1] -= 1

        if recharge[1] <= 0:
            recharge[0] = 0
            recharge[1] = 0
        else:
            mana += recharge[0]
        recharge[1] -= 1

        if boss_hp <= 0:
            return spent_mana

        if strategy[turn] == 'missile':
            if mana < 53:
                return False
            
            spent_mana += 53
            mana -= 53
            boss_hp -= 4

        elif strategy[turn] == 'drain':
            if mana < 73:
                return False

            spent_mana += 73
            mana -= 73
            boss_hp -= 2
            hp += 2

        elif strategy[turn] == 'shield':
            if mana < 113:
                return False

            spent_mana += 113
            mana -= 113
            armor = [7, 6]

        elif strategy[turn] == 'poison':
            if mana < 173:
                return False

            spent_mana += 173
            mana -= 173
            poison = [3, 6]

        elif strategy[turn] == 'recharge':
            if mana < 229:
                return False

            spent_mana += 229
            mana -= 229
            recharge = [101, 5]

        # Boss's turn

        # Tick statuses
        if armor[1] <= 0:
            armor[0] = 0
            armor[1] = 0
        armor[1] -= 1

        if poison[1] <= 0:
            poison[0] = 0
            poison[1] = 0
        else:
            boss_hp -= poison[0]
        poison[1] -= 1

        if recharge[1] <= 0:
            recharge[0] = 0
            recharge[1] = 0
        else:
            mana += recharge[0]
        recharge[1] -= 1

        if boss_hp <= 0:
            return spent_mana

        hp -= max(1, boss_dmg - armor[0])
        if hp <= 0:
            return False

    return False


spells = {'missile': 53,
        'drain': 73,
        'shield': 113,
        'poison': 173,
        'recharge': 229}

# print(simulate_fight(['recharge', 'shield', 'drain', 'poison', 'missile'], 10, 250, 14, 8))

strategies = list(itertools.product(spells.keys(), repeat=9))
min_mana = 99999
for strategy in strategies:
    mana = simulate_fight(strategy, 50, 500, 55, 8)
    if 0 < mana < min_mana:
        min_mana = mana
        print(mana)

# 20:18 (2nd)

# Part 2

# 21:43 (1st)
