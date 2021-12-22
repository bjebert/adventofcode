import hashlib

# Part 1

inp = 'ckczppom'
i = 0
while True:
    tmp = inp + str(i)
    if hashlib.md5(tmp.encode('utf-8')).hexdigest()[:5] == '00000':
        print(i)
        break

    i += 1

# Part 2

i = 0
while True:
    tmp = inp + str(i)
    if hashlib.md5(tmp.encode('utf-8')).hexdigest()[:6] == '000000':
        print(i)
        break

    i += 1
