#!/usr/bin/env python3

# Solution for Advent of Code 2020 Day 05

class BoardingPass:
    s = ""

    def __init__(self, s):
        self.s = s

    def __repr__(self) -> str:
        return self.s

    def getSeatID(self):
        tmp = (self.s
               .replace("L", "0")
               .replace("F", "0")
               .replace("B", "1")
               .replace("R", "1"))

        return int(tmp, 2)


def main():
    input = []
    with open("input.txt") as file:
        input = file.read().strip().split("\n")

    boarding_pass_ids = [BoardingPass(x).getSeatID() for x in input]

    print("part 1: %d" % max(boarding_pass_ids))

    for i in range(min(boarding_pass_ids), max(boarding_pass_ids)):
        if i not in boarding_pass_ids:
            print("part 2: %d" % i)


if __name__ == "__main__":
    main()
