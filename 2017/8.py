from Utils import *

# Part 1

inp = rdl("8.txt")

# inp = """b inc 5 if a > 1
# a inc 1 if b < 5
# c dec -10 if a >= 1
# c inc -20 if c == 10""".split('\n')

reg = dict(zip([x.split()[0] for x in inp], [0 for x in inp]))

max_val = 0

for line in inp:
    ss = line.split()
    instr = ss[1]
    amt = int(ss[2])

    target = ss[4]
    symbol = ss[5]
    target_amt = int(ss[6])

    if symbol == '<':
        if reg[target] < target_amt:
            if instr == 'dec':
                reg[ss[0]] -= amt
            else:
                reg[ss[0]] += amt
    elif symbol == '<=':
        if reg[target] <= target_amt:
            if instr == 'dec':
                reg[ss[0]] -= amt
            else:
                reg[ss[0]] += amt
    elif symbol == '>=':
        if reg[target] >= target_amt:
            if instr == 'dec':
                reg[ss[0]] -= amt
            else:
                reg[ss[0]] += amt
    elif symbol == '>':
        if reg[target] > target_amt:
            if instr == 'dec':
                reg[ss[0]] -= amt
            else:
                reg[ss[0]] += amt
    elif symbol == '!=':
        if reg[target] != target_amt:
            if instr == 'dec':
                reg[ss[0]] -= amt
            else:
                reg[ss[0]] += amt
    elif symbol == '==':
        if reg[target] == target_amt:
            if instr == 'dec':
                reg[ss[0]] -= amt
            else:
                reg[ss[0]] += amt

    max_val = max(max_val, max(list(reg.values())))

print(max(list(reg.values())))

# 7:01 (T-92nd)

# Part 2
print(max_val)

# 7:29 (72nd)
