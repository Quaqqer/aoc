#!/usr/bin/env python3

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 7)

# Main code

crabs = [int(c) for c in puzzle.input_data.split(",")]


def cost_a(pos):
    return sum(abs(c - pos) for c in crabs)


def median(xs):
    xs = sorted(xs)
    if len(xs) % 2 == 0:
        return xs[len(xs) // 2]
    else:
        mi = xs[(len(xs) // 2) - 1]
        ma = xs[len(xs) // 2]
        return (mi + ma) // 2


silver = cost_a(median(crabs))


def tri(n):
    return (n * (n + 1)) // 2


def avg(xs):
    return sum(xs) / len(xs)


def cost_b(pos):
    return sum(tri(abs(c - pos)) for c in crabs)


gold = cost_b(int(avg(crabs)))

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    # puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
