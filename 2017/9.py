from Utils import *

# Part 1

inp = rd("9.txt")


def remove_exclam(s):
    lst = list(s)
    i = 0
    while i < len(lst) - 1:
        if lst[i] == '!':
            del lst[i+1]
        i += 1

    return "".join(lst)


def remove_garbage(s, count=0):
    if '<' in s and '>' in s:
        start = [x for x, y in enumerate(s) if y == '<'][0]
        end = [x for x, y in enumerate(s) if y == '>'][0]

        lst = list(s)
        new_s = "".join(lst[:start] + lst[end+1:])

        return remove_garbage(new_s, count+1)
    else:
        return s, count


def nest(s):
    s = remove_exclam(s)
    s = remove_garbage(s)[0]
    s.replace(',', '')

    count = 0
    level = 0
    for c in s:
        if c == '{':
            level += 1
        elif c == '}':
            count += level
            level -= 1

    return count


print(nest(inp))

# 52:20 (> 100th)

# Part 2

def len_garbage(s):
    s1 = remove_exclam(s)
    s2 = s1.replace('!', '')
    s3 = remove_garbage(s2)

    return len(s2) - len(s3[0]) - s3[1] * 2


print(len_garbage('<>'))
print(len_garbage('<random characters>'))
print(len_garbage('<<<<>'))
print(len_garbage('<{!>}>'))
print(len_garbage('<!!>'))
print(len_garbage('<!!!>>'))
print(len_garbage('<{o"i!a,<{i<a>'))

print(len_garbage('<!!><!!<>'))  # 1

print(len_garbage(inp))

# 1:06:26 (> 100th)