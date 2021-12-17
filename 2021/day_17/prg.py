#!/usr/bin/env python3
import re
from typing import Optional

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 17)

match = re.fullmatch(
    r"target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)", puzzle.input_data
)
assert match

target_min_x = int(match.group(1))
target_max_x = int(match.group(2))
target_min_y = int(match.group(3))
target_max_y = int(match.group(4))


def shoot(xvel, yvel) -> Optional[int]:
    probe_x, probe_y = (0, 0)
    maxy = 0
    while probe_x <= target_max_x and probe_y >= target_min_y:
        probe_x, probe_y = probe_x + xvel, probe_y + yvel
        if probe_y > maxy:
            maxy = probe_y
        xvel = max(0, xvel - 1)
        yvel -= 1

        if (
            target_min_x <= probe_x <= target_max_x
            and target_min_y <= probe_y <= target_max_y
        ):
            return maxy
    return None


bests = []
for x in range(0, target_max_x * 2):
    for y in range(target_min_y, 100):
        s = shoot(x, y)
        if s is not None:
            bests.append(s)

silver = max(bests)
gold = len(bests)

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
