#!/usr/bin/env python3

# Solution for Advent of Code 2020 Day 04

import re


def passport_to_map(passport):
    fields = passport.split()

    fields_map = {}

    for field in fields:
        k = field.split(":")[0]
        v = field.split(":")[1]
        fields_map[k] = v

    return fields_map


def validate_part1(passport):
    fields_map = passport_to_map(passport)

    return ("byr" in fields_map and
            "iyr" in fields_map and
            "eyr" in fields_map and
            "hgt" in fields_map and
            "hcl" in fields_map and
            "ecl" in fields_map and
            "pid" in fields_map)


def validate_part2(passport):
    if not validate_part1(passport):
        return False

    fields_map = passport_to_map(passport)

    if (re.match("^[0-9]{4}$", fields_map["byr"]) == None or
        int(fields_map["byr"]) < 1920 or
            int(fields_map["byr"]) > 2020):
        return False

    if (re.match("^[0-9]{4}$", fields_map["iyr"]) == None or
            int(fields_map["iyr"]) < 2010 or
            int(fields_map["iyr"]) > 2020):
        return False

    if (re.match("^[0-9]{4}$", fields_map["eyr"]) == None or
            int(fields_map["eyr"]) < 2020 or
            int(fields_map["eyr"]) > 2030):
        return False

    if (re.match("^[0-9]*cm$", fields_map["hgt"]) == None and
            re.match("^[0-9]*in$", fields_map["hgt"]) == None):
        return False

    if (re.match("^[0-9]*cm$", fields_map["hgt"]) != None and
        (int(fields_map["hgt"][:-2]) < 150 or
         int(fields_map["hgt"][:-2]) > 193)):
        return False

    if (re.match("^[0-9]*in$", fields_map["hgt"]) != None and
        (int(fields_map["hgt"][:-2]) < 59 or
         int(fields_map["hgt"][:-2]) > 76)):
        return False

    if (re.match("^#[0-9a-f]{6}$", fields_map["hcl"]) == None):
        return False

    if (fields_map["ecl"] not in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]):
        return False

    if (re.match("^[0-9]{9}$", fields_map["pid"]) == None):
        return False

    return True


def main():
    input = []
    with open("input.txt") as file:
        input = file.read().strip().split("\n\n")

    print("part1: %d passports are valid" %
          (sum([1 if validate_part1(p) else 0 for p in input])))
    print("part2: %d passports are valid" %
          (sum([1 if validate_part2(p) else 0 for p in input])))


if __name__ == "__main__":
    main()
