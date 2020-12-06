#!/usr/bin/env python3

# Solution for Advent of Code 2020 Day 02

import re


def is_valid_part1(line):
    m = re.search('^(.*)-(.*) (.*): (.*)$', line)

    min_occur = int(m.group(1))
    max_occur = int(m.group(2))
    pattern = m.group(3)
    password = m.group(4)

    occur = password.count(pattern)

    return min_occur <= occur and occur <= max_occur


def is_valid_part2(line):
    m = re.search('^(.*)-(.*) (.*): (.*)$', line)

    pos1 = int(m.group(1)) - 1
    pos2 = int(m.group(2)) - 1
    pattern = m.group(3)
    password = m.group(4)

    pos1_contains = password[pos1] == pattern
    pos2_contains = password[pos2] == pattern

    return ((pos1_contains and not pos2_contains) or
            (not pos1_contains and pos2_contains))


def main():
    input = []
    with open("input.txt") as file:
        input = file.read().strip().split("\n")

    print("part1: %d passwords are valid"
          % sum([1 if is_valid_part1(x) else 0 for x in input]))
    print("part2: %d passwords are valid"
          % sum([1 if is_valid_part2(x) else 0 for x in input]))


if __name__ == "__main__":
    main()
