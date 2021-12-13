#!/usr/bin/env python3
import re

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 13)
choord_input, fold_input = tuple(puzzle.input_data.split("\n\n"))


# Main code

# get coordinates
coords = set()
for line in choord_input.split("\n"):
    x, y = tuple(map(int, line.split(",")))
    coords.add((x, y))


def fold(choords, axis, v):
    nc = set()
    if axis == "x":
        for (x, y) in choords:
            if x < v:
                nc.add((x, y))
            elif x > v:
                nc.add((v * 2 - x, y))
    else:
        assert axis == "y"
        for (x, y) in choords:
            if y < v:
                nc.add((x, y))
            elif y > v:
                nc.add((x, v * 2 - y))
    return nc


# do folds
reg = re.compile("fold along (x|y)=([0-9]+)")
for line in fold_input.split("\n"):
    match = reg.match(line)
    assert match
    axis, v = match.group(1), int(match.group(2))

    coords = fold(coords, axis, v)

    if "silver" not in locals():
        silver = len(coords)


# print coords
maxx, maxy = 0, 0
for (x, y) in coords:
    maxx = max(maxx, x)
    maxy = max(maxy, y)
for y in range(maxy + 1):
    print("".join("█" if (x, y) in coords else " " for x in range(maxx + 1)))

# get gold from user input
gold = input("Please enter the letters that you can see above: ").upper()


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
