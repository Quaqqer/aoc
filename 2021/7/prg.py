#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 7)

# Main code

crabs = [int(c) for c in puzzle.input_data.split(",")]


def cost_a(pos):
    return sum(abs(c - pos) for c in crabs)


silver = min(cost_a(pos) for pos in range(min(crabs), max(crabs) + 1))


def tri(n):
    return (n * (n + 1)) // 2


def cost_b(pos):
    return sum(tri(abs(c - pos)) for c in crabs)


gold = min(cost_b(pos) for pos in range(min(crabs), max(crabs) + 1))

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    # puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
