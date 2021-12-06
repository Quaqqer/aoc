#!/usr/bin/env python3
from collections import defaultdict

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 6)
lines = puzzle.input_data.split("\n")

# Main code

fish = defaultdict(int)
for f in lines[0].split(","):
    f = int(f)
    fish[f] += 1


def stepped(fish):
    new_fish = defaultdict(int)
    for timer, amt in fish.items():
        if timer == 0:
            new_fish[6] += amt
            new_fish[8] += amt
        else:
            new_fish[timer - 1] += amt
    return new_fish


for _ in range(80):
    fish = stepped(fish)
silver = sum(fish.values())

for _ in range(256 - 80):
    fish = stepped(fish)
gold = sum(fish.values())


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
