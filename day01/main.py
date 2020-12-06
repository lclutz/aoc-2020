#!/usr/bin/env python3

# Solution for Advent of Code 2020 Day 01

import itertools


def part1(input):

    for x, y in itertools.combinations(input, 2):
        if x + y == 2020:
            return x * y


def part2(input):

    for x, y, z in itertools.combinations(input, 3):
        if x + y + z == 2020:
            return x * y * z


def main():

    input = []

    with open("input.txt") as file:
        input = [int(x) for x in file.read().strip().split("\n")]

    print("part1: %d" % part1(input))
    print("part2: %d" % part2(input))


if __name__ == "__main__":
    main()
