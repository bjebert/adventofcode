from Utils import *

# Part 1

inp = rdl("10.txt")

score = {')': 3, ']': 57, '}': 1197, '>': 25137}


def is_corrupt(line):
    op = {'<': [], '(': [], '{': [], '[': []}

    for i in range(len(line)):
        c = line[i]

        if c in op:
            op[c].append(i)
        else:
            # Close chunk and ensure that count is even inside
            if c == '>':
                if not len(op['<']):
                    return True

                start = op['<'].pop()
            elif c == ')':
                if not len(op['(']):
                    return True

                start = op['('].pop()
            elif c == '}':
                if not len(op['{']):
                    return True

                start = op['{'].pop()
            elif c == ']':
                if not len(op['[']):
                    return True

                start = op['['].pop()

            chunk = line[start:(i + 1)]

            if chunk.count('(') != chunk.count(')') or chunk.count('<') != chunk.count('>') or chunk.count(
                    '{') != chunk.count('}') or chunk.count('[') != chunk.count(']'):
                # corrupted
                return True
    return op


cnt = []
for line in inp:
    op = is_corrupt(line)
    if type(op) == dict:
        score = 0

        while any([len(x) > 0 for x in op.values()]):
            # Find highest index and pop
            max_i = -1

            to_pop = None
            for k in op:
                if len(op[k]) and max(op[k]) > max_i:
                    to_pop = k
                    max_i = max(op[k])

            op[to_pop].pop()
            val = 0
            if to_pop == '(':
                val = 1
            elif to_pop == '[':
                val = 2
            elif to_pop == '{':
                val = 3
            elif to_pop == '<':
                val = 4

            score *= 5
            score += val

        cnt.append(score)

print(sorted(cnt)[len(cnt)//2])



