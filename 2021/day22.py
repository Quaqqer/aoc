#!/usr/bin/env python3
import re

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 22)
lines = puzzle.input_data.split("\n")
regex = re.compile(r"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)")


def parse(line):
    match = regex.match(line)
    assert match
    onoff = True if match.group(1) == "on" else False
    minx, maxx = int(match.group(2)), int(match.group(3))
    miny, maxy = int(match.group(4)), int(match.group(5))
    minz, maxz = int(match.group(6)), int(match.group(7))
    return onoff, minx, maxx, miny, maxy, minz, maxz


def lparse(line):
    onoff, minx, maxx, miny, maxy, minz, maxz = parse(line)
    return (
        onoff,
        max(minx, -50),
        min(maxx, 50),
        max(miny, -50),
        min(maxy, 50),
        max(minz, -50),
        min(maxz, 50),
    )


a_onoffs = dict()

for line in lines:
    onoff, minx, maxx, miny, maxy, minz, maxz = lparse(line)
    for x in range(minx, maxx + 1):
        for y in range(miny, maxy + 1):
            for z in range(minz, maxz + 1):
                a_onoffs[(x, y, z)] = onoff

silver = sum(a_onoffs.values())


class Cube:
    def __init__(self, x, y, z, width, height, depth, positive=True):
        self.x = x
        self.y = y
        self.z = z
        self.width = width
        self.height = height
        self.depth = depth
        self.positive = positive

    def size(self) -> int:
        return (1 if self.positive else -1) * self.width * self.height * self.depth

    def overlap(self, other):
        x = max(self.x, other.x)
        y = max(self.y, other.y)
        z = max(self.z, other.z)
        x2 = min(self.x + self.width, other.x + other.width)
        y2 = min(self.y + self.height, other.y + other.height)
        z2 = min(self.z + self.depth, other.z + other.depth)

        if x2 - x <= 0 or y2 - y <= 0 or z2 - z <= 0:
            return None

        return Cube(x, y, z, x2 - x, y2 - y, z2 - z, self.positive != other.positive)

    def __repr__(self):
        return (
            ("+" if self.positive else "-")
            + f" x={self.x}..{self.x + self.width - 1}"
            + f" y={self.y}..{self.y + self.height - 1}"
            + f" z={self.z}..{self.z + self.depth - 1}"
        )


cubes = []
for line in lines:
    onoff, minx, maxx, miny, maxy, minz, maxz = parse(line)
    cube = Cube(
        minx, miny, minz, maxx - minx + 1, maxy - miny + 1, maxz - minz + 1
    )

    for i in range(len(cubes)):
        overlap = cube.overlap(cubes[i])
        if overlap:
            cubes.append(overlap)

    if onoff:
        cubes.append(cube)

gold = sum(map(lambda c: c.size(), cubes))


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
