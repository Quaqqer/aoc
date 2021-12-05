#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 5)
lines = puzzle.input_data.split("\n")

# Main code

import re
from collections import defaultdict

regex = re.compile("([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)")

intersects = defaultdict(int)

for line in lines:
    match = regex.fullmatch(line)
    assert match

    x1, y1, x2, y2 = tuple(int(v) for v in match.groups())

    if x1 == x2:
        for y in range(min(y1, y2), max(y1, y2) + 1):
            key = (x1, y)
            intersects[key] += 1
    elif y1 == y2:
        for x in range(min(x1, x2), max(x1, x2) + 1):
            key = (x, y1)
            intersects[key] += 1

silver = sum(map(lambda v: v >= 2, intersects.values()))

intersects = defaultdict(int)

for line in lines:
    match = regex.match(line)
    assert match

    x1, y1, x2, y2 = tuple(int(v) for v in match.groups())

    if x1 == x2:
        for y in range(min(y1, y2), max(y1, y2) + 1):
            key = (x1, y)
            intersects[key] += 1
    else:
        k = (y2 - y1) // (x2 - x1)
        m = y1 - x1 * k

        for x in range(min(x1, x2), max(x1, x2) + 1):
            key = (x, k*x + m)
            intersects[key] += 1

gold = sum(map(lambda v: v >= 2, intersects.values()))

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}") # type: ignore
    puzzle.answer_a = silver # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}") # type: ignore
    puzzle.answer_b = gold # type: ignore
