#!/usr/bin/env python3

# Solution for Advent of Code 2020 Day 03

def part1(input):
    result = 0
    width = len(input[0])
    y = 1
    x = 3 % width

    while(y < len(input)):
        if (input[y][x] == '#'):
            result += 1
        y += 1
        x = (x + 3) % width

    return result


def part2(input, dy, dx):
    result = 0
    width = len(input[0])
    y = dy
    x = dx % width

    while(y < len(input)):
        if (input[y][x] == '#'):
            result += 1
        y += dy
        x = (x + dx) % width

    return result


def mul(list):
    result = 1
    for entry in list:
        result *= entry
    return result


def main():
    input = []
    with open("input.txt") as file:
        input = file.read().strip().split("\n")

    print("part1: %d trees encountered" % part1(input))

    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

    print("part2: %d" % (mul([part2(input, s[1], s[0]) for s in slopes])))


if __name__ == "__main__":
    main()
