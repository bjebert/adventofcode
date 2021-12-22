# Part 1

with open('2.txt', 'r') as f:
    inp = f.readlines()

    inp = [i.replace('\n', '') for i in inp]

    l = [int(i.split('x')[0]) for i in inp]
    w = [int(i.split('x')[1]) for i in inp]
    h = [int(i.split('x')[2]) for i in inp]

    dim = [(l[x], w[x], h[x]) for x in range(len(inp))]

    print(sum([2*i[0]*i[1] + 2*i[1]*i[2] + 2*i[0]*i[2] + min((i[0]*i[1], i[1]*i[2], i[0]*i[2])) for i in dim]))

# Part 2

with open('2.txt', 'r') as f:
    inp = f.readlines()

    inp = [i.replace('\n', '') for i in inp]

    l = [int(i.split('x')[0]) for i in inp]
    w = [int(i.split('x')[1]) for i in inp]
    h = [int(i.split('x')[2]) for i in inp]

    dim = [(l[x], w[x], h[x]) for x in range(len(inp))]

    print(sum([i[0]*i[1]*i[2] + min((2*(i[0]+i[1]), 2*(i[1]+i[2]), 2*(i[0]+i[2]))) for i in dim]))
