def rd(filename):
    with open(filename, 'r') as f:
        return f.read().replace('\n', '')


def rdl(filename):
    with open(filename, 'r') as f:
        return [line.replace('\n', '') for line in f.readlines()]