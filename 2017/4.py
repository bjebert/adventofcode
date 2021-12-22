from Utils import *

# Part 1

inp = rdl("4.txt")


def is_valid(pw):
    x = pw.split(' ')
    return len(x) == len(unique(x))


print(sum([is_valid(pw) for pw in inp]))

# 0:48 (T-4th)

# Part 2


def is_valid2(pw):
    x = pw.split(' ')
    if len(x) != len(unique(x)):
        return False

    return len(unique([sorted(list(x)) for x in pw.split(' ')])) == len([sorted(list(x)) for x in pw.split(' ')])


print(sum([is_valid2(pw) for pw in inp]))

# 2:10 (T-26th)
