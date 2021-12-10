#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 1)
lines = puzzle.input_data.split("\n")


# Main code
def increases(pairs):
    return sum(map(lambda p: p[1] > p[0], pairs))


def pairs(lst):
    return [(lst[i], lst[i + 1]) for i in range(len(lst) - 1)]


depths = [int(line) for line in lines]

silver = increases(pairs(depths))

three_sums = [sum(depths[i:i + 3]) for i in range(0, len(depths) - 2)]
gold = increases(pairs(three_sums))

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")
    puzzle.answer_a = silver
if "gold" in locals():
    print(f"Gold: {gold}")
    puzzle.answer_b = gold
