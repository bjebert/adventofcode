class Group:
    def __init__(self, i, side, line, boost=0):
        self.i = i
        self.side = side

        attribs, attack = line.split(';')
        units, hp, *type_mods = attribs.split()
        units = int(units)
        hp = int(hp)
        weak = []
        immune = []
        cur = None
        for w in type_mods:
            if w == "weak":
                cur = weak
            elif w == "immune":
                cur = immune
            else:
                cur.append(w)

        self.units = units
        self.hp = hp
        self.weak = weak
        self.immune = immune

        attack_amount, attack_type, initiative = attack.split()
        attack_amount = int(attack_amount)
        initiative = int(initiative)

        self.attack = attack_amount + boost
        self.attack_type = attack_type
        self.initiative = initiative

        self.attacker = None
        self.target = None

    def clear(self):
        self.attacker = None
        self.target = None

    def choose(self, groups):
        assert self.target is None
        cands = [group for group in groups
                 if group.side != self.side
                 and group.attacker is None
                 and self.damage_prio(group)[0] > 0]
        if cands:
            self.target = max(cands, key=lambda group: self.damage_prio(group))
            assert self.target.attacker is None
            self.target.attacker = self

    def effective_power(self):
        return self.units * self.attack

    def target_prio(self):
        return (-self.effective_power(), -self.initiative)

    def damage_prio(self, target):
        if target.units == 0:
            return (0, 0, 0)
        if self.attack_type in target.immune:
            return (0, 0, 0)
        mul = 1
        if self.attack_type in target.weak:
            mul = 2
        return (mul * self.units * self.attack, target.effective_power(), target.initiative)

    def do_attack(self, target):
        total_attack = self.damage_prio(target)[0]
        killed = total_attack // target.hp
        target.units = max(0, target.units - killed)
        return killed

    def __repr__(self):
        return f"{'immune' if not self.side else 'infection'} {self.i}"


immune_system_input = """17 5390 weak radiation bludgeoning;4507 fire 2
989 1274 immune fire weak bludgeoning slashing;25 slashing 3"""

infection_input = """801 4706 weak radiation;116 bludgeoning 1
4485 2961 immune radiation weak fire cold;12 slashing 4"""

immune_system_input = """1514 8968 weak cold;57 bludgeoning 9
2721 6691 weak cold;22 slashing 15
1214 10379 immune bludgeoning;69 fire 16
2870 4212;11 radiation 12
1239 5405 weak cold;37 cold 18
4509 4004 immune radiation weak cold;8 slashing 20
3369 10672 weak slashing;29 cold 11
2890 11418 immune bludgeoning weak fire; 30 cold 8
149 7022 weak slashing;393 radiation 13
2080 5786 immune slashing bludgeoning weak fire;20 fire 7"""

infection_input = """817 47082 immune slashing radiation bludgeoning;115 cold 3
4183 35892;16 bludgeoning 1
7006 11084;2 fire 2
4804 25411;10 cold 14
6262 28952 weak fire;7 slashing 10
628 32906 weak slashing;99 radiation 4
5239 46047 immune fire;14 bludgeoning 6
1173 32300 weak cold slashing;53 bludgeoning 19
3712 12148 weak slashing immune cold;5 slashing 17
334 43582 weak cold fire;260 cold 5"""


def solve(boost):
    immune_system_groups = []
    infection_groups = []

    i = 0
    for line in immune_system_input.split("\n"):
        i += 1
        immune_system_groups.append(Group(i, False, line, boost))

    i = 0
    for line in infection_input.split("\n"):
        i += 1
        infection_groups.append(Group(i, True, line))

    groups = immune_system_groups + infection_groups

    old = (-1, -1)
    while True:
        for f in immune_system_groups:
            print(f'{repr(f)} contains {f.units}')
        for f in infection_groups:
            print(f'{repr(f)} contains {f.units}')

        print('\n')

        groups = sorted(groups, key=lambda group: group.target_prio())
        for group in groups:
            group.clear()
        for group in groups:
            group.choose(groups)
        groups = sorted(groups, key=lambda group: -group.initiative)
        for group in groups:
            if group.target:
                killed = group.do_attack(group.target)
                print(f'{repr(group)} kills {repr(group.target)} {killed}')

        print('\n')

        immune_system_units = sum(group.units for group in groups if group.side == False)
        infection_units = sum(group.units for group in groups if group.side == True)
        if (immune_system_units, infection_units) == old:
            return (immune_system_units, infection_units)
        old = (immune_system_units, infection_units)


# star 1
print(solve(0)[1])