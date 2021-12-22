from Utils import *

# Part 1

a = 289
b = 629

count = 0

for i in range(40000000):
    a = (a * 16807) % 2147483647
    b = (b * 48271) % 2147483647

    bin_a = f'{a:16b}'[::-1][:16][::-1]
    bin_b = f'{b:16b}'[::-1][:16][::-1]

    if bin_a == bin_b:
        count += 1

print(count)

# 6:15 (> 100th)

count = 0

a = 289
b = 629

bins_a = []
bins_b = []

while len(bins_b) < 5000000:
    a = (a * 16807) % 2147483647
    b = (b * 48271) % 2147483647

    if a % 4 == 0:
        bins_a.append(f'{a:16b}'[::-1][:16][::-1])

    if b % 8 == 0:
        bins_b.append(f'{b:16b}'[::-1][:16][::-1])

    if len(bins_b) % 200000 == 0:
        print(len(bins_b))

for j in range(min(len(bins_a), len(bins_b))):
    if bins_a[j] == bins_b[j]:
        count += 1

print(count)

# 15:23 (> 100th)