from Utils import *
inp = rdl("10.txt")

m = {'(': ')', '[': ']', '{': '}', '<': '>'}
s = {')': 1, ']': 2, '}': 3, '>': 4}

cnt = []
for line in inp:
    st = []
    cor = False
    for c in line:
        if c in m:
            st.append(m[c])
        elif c == st[-1]:
            st.pop()
        else:
            cor = True
            break
    if not cor:
        score = 0
        while len(st):
            c = st.pop()
            score *= 5
            score += s[c]
        cnt.append(score)

print(sorted(cnt)[len(cnt)//2])

