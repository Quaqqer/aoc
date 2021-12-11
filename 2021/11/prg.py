#!/usr/bin/env python3
from copy import deepcopy

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 11)
lines = puzzle.input_data.split("\n")


# Main code
octopuses = [[int(col) for col in row] for row in lines]


def all_cells(lst):
    for row in range(len(lst)):
        for col in range(len(lst[0])):
            yield (row, col)


def flash(octopuses, row, col):
    for dr in range(-1, 1 + 1):
        for dc in range(-1, 1 + 1):
            nr, nc = row + dr, col + dc
            if dr == 0 and dc == 0:
                octopuses[nr][nc] = 11
            elif 0 <= nr < len(octopuses) and 0 <= nc < len(octopuses[0]):
                if octopuses[nr][nc] <= 9:
                    octopuses[nr][nc] += 1


def step(octopuses):
    for (row, col) in all_cells(octopuses):
        octopuses[row][col] += 1
    f = True
    flashes = 0
    while f:
        f = False
        for (row, col) in all_cells(octopuses):
            if octopuses[row][col] == 10:
                f = True
                flashes += 1
                flash(octopuses, row, col)
    for (row, col) in all_cells(octopuses):
        if octopuses[row][col] == 11:
            octopuses[row][col] = 0
    return flashes


silver_octopuses = deepcopy(octopuses)
flashes = 0
for i in range(100):
    flashes += step(silver_octopuses)
silver = flashes


gold_octopuses = deepcopy(octopuses)
_step = 0
gold = None
while gold is None:
    flashes = step(gold_octopuses)
    _step += 1
    if flashes == (len(gold_octopuses) * len(gold_octopuses[0])):
        gold = _step

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
