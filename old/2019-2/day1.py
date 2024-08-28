#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2019, 1)
lines = puzzle.input_data.split("\n")


def fuel_for(mass: int, recursive: bool) -> int:
    fuel = mass // 3 - 2
    if fuel <= 0:
        return 0
    elif recursive:
        return fuel + fuel_for(fuel, recursive)
    else:
        return fuel


masses = [int(line) for line in lines]

silver = sum(fuel_for(mass, False) for mass in masses)

gold = sum(fuel_for(mass, True) for mass in masses)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
