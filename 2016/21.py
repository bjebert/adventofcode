from Utils import *

# Part 1

inp = rdl("21.txt")
start = 'abcdefgh'


def scramble(start):
    for line in inp:
        ss = line.split(' ')

        if ss[0] == 'move':
            pos_X = int(ss[2])
            pos_Y = int(ss[-1])

            c = start[pos_X]
            lst = [y for x, y in enumerate(start) if x != pos_X]
            lst.insert(pos_Y, c)
            start = "".join(lst)

        elif ss[0] == 'rotate':
            if ss[1] == 'left':
                steps = int(ss[2])
                start = start[(steps % len(start)):] + start[0:(steps % len(start))]
            elif ss[1] == 'right':
                steps = int(ss[2])
                start = start[-(steps % len(start)):] + start[0:-(steps % len(start))]
            else:
                position = ss[-1]
                idx = [x for x, y in enumerate(start) if y == position][0]

                steps = 1 + idx

                if idx >= 4:
                    steps += 1

                start = start[-(steps % len(start)):] + start[0:-(steps % len(start))]

        elif ss[0] == 'swap':
            if ss[1] == 'position':
                pos_l = int(ss[2])
                pos_r = int(ss[-1])

                new = ''
                for i in range(len(start)):
                    if i == pos_l:
                        j = pos_r
                    elif i == pos_r:
                        j = pos_l
                    else:
                        j = i

                    new += start[j]
                start = new

            elif ss[1] == 'letter':
                let_x = ss[2]
                let_y = ss[-1]

                start = start.replace(let_x, '!')
                start = start.replace(let_y, let_x)
                start = start.replace('!', let_y)

        elif ss[0] == 'reverse':
            pos_X = int(ss[2])
            pos_Y = int(ss[-1])
            start = start[:pos_X] + start[pos_X:(pos_Y+1)][::-1] + start[(pos_Y+1):]
    return start


# print(scramble(start))

# 22:01 (49th)

unscramble = 'fbgdceah'

x = list(permutations(list('abcdefgh'), 8))
x = [''.join(y) for y in x]

for start in x:
    if scramble(start) == unscramble:
        print(start)

# 25:42 (15th)