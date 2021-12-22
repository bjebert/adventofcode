inp = "1321131112"

# Part 1

for i in range(40):  # iter
    new_inp = ""
    j = 0
    while j < len(inp):
        c = inp[j]

        # Lookahead, how many of the same character is c
        k = j + 1
        consec = 1
        while k < len(inp):
            if inp[k] == c:
                consec += 1
                j += 1
            else:
                break
            k += 1

        new_inp += str(consec) + c
        j += 1
    inp = new_inp

print(len(inp))

# 10:02 (90th)

# Part 2

inp = "1321131112"

for i in range(50):  # iter
    new_inp = ""
    j = 0
    while j < len(inp):
        c = inp[j]

        # Lookahead, how many of the same character is c
        k = j + 1
        consec = 1
        while k < len(inp):
            if inp[k] == c:
                consec += 1
                j += 1
            else:
                break
            k += 1

        new_inp += str(consec) + c
        j += 1
    inp = new_inp

print(len(inp))

# 10:43 (77th)
