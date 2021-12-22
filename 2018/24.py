from Utils import *

# Part 1

inp = rdl("24.txt")
immune_raw = inp[1:11]
infection_raw = inp[13:]

# inp = """Immune System:
# 17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
# 989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3
#
# Infection:
# 801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
# 4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4""".split('\n')
# immune_raw = inp[1:3]
# infection_raw = inp[5:]


class Unit:
    def __init__(self, group, desc, side, units, hp, immunity, weakness, damage, dtype, initiative):
        self.group = group
        self.desc = desc
        self.side = side
        self.units = units
        self.hp = hp
        self.immunity = immunity
        self.weakness = weakness
        self.damage = damage
        self.dtype = dtype
        self.initiative = initiative

    def power(self):
        if self.units <= 0:
            return 0

        return self.units * self.damage

    def __repr__(self):
        return f'{self.__str__()} ({self.units} units), ({self.power()} power)'

    def __str__(self):
        return str(self.side) + " " + str(self.group)


def parse_input(boost=0):
    immune = []
    infection = []
    i = 1
    for line in immune_raw:
        ls = line.split()
        if 'immune' in line:
            immunities = [x.strip() for x in line.split('immune to ')[1].split(';')[0].split(')')[0].split(',')]
        else:
            immunities = []

        if 'weak' in line:
            weaknesses = [x.strip() for x in line.split('weak to ')[1].split(';')[0].split(')')[0].split(',')]
        else:
            weaknesses = []

        dmg = int(line.split('that does ')[1].split()[0])
        dt = line.split('damage')[0].split()[-1]

        unit = Unit(i, line, "immune", int(ls[0]), int(ls[4]), immunities, weaknesses, dmg + boost, dt, int(line.split()[-1]))
        immune.append(unit)
        i += 1

    i = 1
    for line in infection_raw:
        ls = line.split()
        if 'immune' in ls[7]:
            immunities = [x.strip() for x in line.split('immune to ')[1].split(';')[0].split(')')[0].split(',')]
        else:
            immunities = []

        if 'weak' in line:
            weaknesses = [x.strip() for x in line.split('weak to ')[1].split(';')[0].split(')')[0].split(',')]
        else:
            weaknesses = []

        dmg = int(line.split('that does ')[1].split()[0])
        dt = line.split('damage')[0].split()[-1]

        unit = Unit(i, line, "infection", int(ls[0]), int(ls[4]), immunities, weaknesses, dmg, dt,
                    int(line.split()[-1]))
        infection.append(unit)
        i += 1

    return immune, infection


def get_damage(fighter, target):
    if fighter.dtype in target.immunity:
        return 0
    elif fighter.dtype in target.weakness:
        return 2 * fighter.power()
    else:
        return fighter.power()


def remaining_units(target, dmg):
    health = target.units * target.hp

    if dmg >= health:
        return 0
    elif dmg <= 0:
        return target.units
    else:
        return target.units - (dmg // target.hp)


def fight(boost):
    immune, infection = parse_input(boost)
    iter = 0
    while any([x.units > 0 for x in immune]) and any([x.units > 0 for x in infection]):
        iter += 1
        # Target selection ---
        fighters = immune + infection

        fighters.sort(key=lambda x: (-x.power(), -x.initiative))
        targets = dict()

        for f in fighters:
            target = None
            max_dmg = -1

            if f.units <= 0:
                continue

            if f.side == 'immune':
                for t in infection:
                    if t in targets.values() or t.units <= 0:
                        continue
                    dmg = get_damage(f, t)
                    if dmg > max_dmg and dmg > 0:
                        target = t
                        max_dmg = dmg
                    elif dmg == max_dmg:
                        if t.power() > target.power():
                            target = t
                        elif t.power() == target.power():
                            if t.initiative > target.initiative:
                                target = t
            else:
                for t in immune:
                    if t in targets.values() or t.units <= 0:
                        continue
                    dmg = get_damage(f, t)
                    if dmg > max_dmg and dmg > 0:
                        target = t
                        max_dmg = dmg
                    elif dmg == max_dmg:
                        if t.power() > target.power():
                            target = t
                        elif t.power() == target.power():
                            if t.initiative > target.initiative:
                                target = t

            targets[str(f)] = target

        # Attacking ---
        attackers = immune + infection
        attackers.sort(key=lambda x: -x.initiative)

        for a in attackers:
            if a.units > 0 and targets[str(a)]:
                target = targets[str(a)]
                dmg = get_damage(a, target)
                start = target.units
                target.units = remaining_units(target, dmg)
                # print(f'{str(a)} kills {str(target)} {start - target.units}')

        if iter > 10000:
            # Draw
            return False, sum([x.units for x in infection])
    if sum([x.units for x in immune]):
        return True, sum([x.units for x in immune])
    else:
        return False, sum([x.units for x in infection])

# Part 1
# 1:41:57


# Part 2
lo, hi = 0, 250

while lo != hi:
    boost = math.ceil((lo + hi) / 2)
    result = fight(boost)

    print(lo, boost, hi, result)

    if result[0]:
        hi = boost
    else:
        lo = boost

print(fight(84))  # 1:51:25




