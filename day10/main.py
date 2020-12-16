#!/usr/bin/env python3

example1 = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

example2 = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45,
            19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]

input = [99, 151, 61, 134, 112, 70, 75, 41, 119, 137, 158, 50, 167, 60, 116,
         117, 62, 82, 31, 3, 72, 88, 165, 34, 8, 14, 27, 108, 166, 71, 51, 42,
         135, 122, 140, 109, 1, 101, 2, 77, 85, 76, 143, 100, 127, 7, 107, 13,
         148, 118, 56, 159, 133, 21, 154, 152, 130, 78, 54, 104, 160, 153, 95,
         49, 19, 69, 142, 63, 11, 12, 29, 98, 84, 28, 17, 146, 161, 115, 4, 94,
         24, 126, 136, 91, 57, 30, 155, 79, 66, 141, 48, 125, 162, 37, 40, 147,
         18, 20, 45, 55, 83]

cache = {}

for a in [example1, example2, input]:
    a.append(0)
    a.append(max(a) + 3)


def count_combinations(a, n):

    if n == 0:
        return 1

    if n in cache:
        return cache[n]

    result = 0
    for prev_n in a:
        if n - prev_n <= 3 and n - prev_n > 0:
            result += count_combinations(a, prev_n)

    cache[n] = result

    return result


cache = {}
print("example1: %d" % count_combinations(example1, max(example1)))

cache = {}
print("example2: %d" % count_combinations(example2, max(example2)))

cache = {}
print("input: %d" % count_combinations(input, max(input)))
