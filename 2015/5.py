import re

# Part 1

with open('5.txt', 'r') as f:
    inp = f.readlines()
    inp = [i.replace('\n', '') for i in inp]

    print(len([s for s in inp if re.search(r'[aeiou].*[aeiou].*[aeiou]', s) and
               re.search(r'(.)\1', s) and not
               re.search('(ab)|(cd)|(pq)|(xy)', s)]))

# 4:33 (11th)

# Part 2

with open('5.txt', 'r') as f:
    inp = f.readlines()
    inp = [i.replace('\n', '') for i in inp]

    print(sum([re.search(r'(..).*\1', s) is not None and re.search(r'(.).\1', s) is not None for s in inp]))

# 48:00 (> 100th)
