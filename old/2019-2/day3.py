#!/usr/bin/env python3
import re

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2019, 3)
lines = puzzle.input_data.split("\n")


def get_wire(moves: list[str]) -> dict[tuple[int, int], int]:
    pos = (0, 0)
    dist = 1

    positions = {}

    for move in moves:
        match = re.match(r"(.)(\d+)", move)
        assert match
        dir, stepss = match.groups()
        steps = int(stepss)

        delta = {"U": (0, -1), "D": (0, 1), "L": (-1, 0), "R": (1, 0)}[dir]
        for _ in range(steps):
            pos = pos[0] + delta[0], pos[1] + delta[1]
            if pos not in positions:
                positions[pos] = dist
            dist += 1

    return positions


wire_a = get_wire(lines[0].split(","))
wire_b = get_wire(lines[1].split(","))

dists = {}
for pos in set(wire_a.keys()).union(set(wire_b.keys())):
    if pos in wire_a and pos in wire_b:
        dists[pos] = wire_a[pos] + wire_b[pos]

silver = min(abs(a) + abs(b) for (a, b) in dists.keys())
gold = min(dists.values())


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
