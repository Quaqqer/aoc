#!/usr/bin/env python3
import re

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 13)
id = puzzle.input_data
choord_lines, fold_lines = tuple(id.split("\n\n"))


# Main code
choords = set()

reg = re.compile("(.*),(.*)")
for line in choord_lines.split("\n"):
    match = reg.fullmatch(line)
    assert match
    x, y = int(match.group(1)), int(match.group(2))

    choords.add((x, y))


def fold(choords, axis, v):
    nc = set()
    if axis == "x":
        for (x, y) in choords:
            if x < v:
                nc.add((x, y))
            elif x > v:
                nc.add((v + (v - x), y))
    else:
        assert axis == "y"
        for (x, y) in choords:
            if y < v:
                nc.add((x, y))
            elif y > v:
                nc.add((x, v + (v - y)))
    return nc


reg = re.compile("fold along (x|y)=(.*)")
for line in fold_lines.split("\n"):
    print(line)
    match = reg.match(line)
    assert match
    axis = match.group(1)
    v = int(match.group(2))

    choords = fold(choords, axis, v)

    if "silver" not in locals():
        silver = len(choords)

maxx, maxy = 0, 0
for (x, y) in choords:
    maxx = max(maxx, x)
    maxy = max(maxy, y)

for y in range(maxy + 1):
    print("".join("█" if (x, y) in choords else " " for x in range(maxx + 1)))

gold = input("Please enter the letters that you can see above: ")


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
