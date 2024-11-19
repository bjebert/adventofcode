target_row = 2981
target_col = 3075

curr = 20151125
row = 1
col = 1


while True:
    if row == 1:
        row = col + 1
        col = 1
    else:
        row -= 1
        col += 1

    curr = (curr * 252533) % 33554393
    if col == target_col and row == target_row:
        print(curr)
        break



