#!/usr/bin/env python3
import functools
import re
from collections import defaultdict

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2019, 6)
lines = puzzle.input_data.split("\n")


# Main code
orbits: dict[str, set[str]] = defaultdict(set)
for line in lines:
    m = re.match(r"(.*)\)(.*)", line)
    assert m
    orbit, orbiter = m.groups()
    orbits[orbit].add(orbiter)


@functools.cache
def orbiters(orbit: str) -> int:
    if orbit in orbits:
        return sum(1 + orbiters(o) for o in orbits[orbit])
    return 0


silver = sum(orbiters(o) for o in orbits)


def path(pos) -> list[str]:
    reverse_edges = {o: i for i, oo in orbits.items() for o in oo}
    curr = pos
    path = [curr]
    while curr != "COM":
        curr = reverse_edges[curr]
        path.append(curr)
    return path[::-1]


lpath = path("YOU")
rpath = path("SAN")

i = 0
while lpath[i] == rpath[i]:
    i += 1
gold = len(lpath) + len(rpath) - 2 * i - 2


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
