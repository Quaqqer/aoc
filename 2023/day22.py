# 22   01:06:32  1522      0   01:30:57  1467      0
# Slow today again, but it was a fun puzzle!

import functools
from collections import deque

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("22"))
data = puzzle.input_data
lines = data.splitlines()

Cube: TypeAlias = tuple[Range, Range, Range]


def visualize(cubes: list[Cube]):
    """Visualize the cubes, only works for test data because the alphabet is limited"""
    points_x: dict[Coord, str] = {}
    points_z: dict[Coord, str] = {}

    for ci, cube in enumerate(cubes):
        cx, cy, cz = cube
        for x in range(cx.start, cx.end):
            for z in range(cz.start, cz.end):
                if (x, z) in points_x:
                    points_x[x, z] = "?"
                else:
                    points_x[x, z] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[ci]

        for y in range(cy.start, cy.end):
            for z in range(cz.start, cz.end):
                if (y, z) in points_z:
                    points_z[y, z] = "?"
                else:
                    points_z[y, z] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[ci]
    print_set_grid(points_x)  # type: ignore
    print()
    print_set_grid(points_z)  # type: ignore


def drop(i: int):
    """Drop cube i. O(n)"""
    old = cubes[i]
    x, y, z = old

    # Find the floors below us, the bottom is always 0
    floors = [0]
    for j in range(len(cubes)):
        if i == j:
            continue

        x2, y2, z2 = cubes[j]
        # If we overlap in x and y we can stand on cube j (if it is below us)
        if x.overlap(x2) is not None and y.overlap(y2) is not None:
            # Check if it is below us, then add it to the list of floors
            if z2.end <= z.start:
                floors.append(z2.end)

    new_z = max(floors)

    delta = new_z - z.start

    new = x, y, z + delta
    cubes[i] = new


def is_on(a: Cube, b: Cube) -> bool:
    """Return true iff a is on b"""
    ax, ay, az = a
    bx, by, bz = b
    return (
        az.start == bz.end and ax.overlap(bx) is not None and ay.overlap(by) is not None
    )


@functools.cache
def ons(i: int) -> list[int]:
    """Cubes that are on i"""
    return [j for j in range(len(cubes)) if i != j and is_on(cubes[j], cubes[i])]


@functools.cache
def belows(i: int) -> list[int]:
    """Cubes that are below i"""
    return [j for j in range(len(cubes)) if i != j and is_on(cubes[i], cubes[j])]


@functools.cache
def amt_would_drop(start: int):
    """Get the cubes that would drop if start was removed

    Search with a bfs
    """
    visited = {start}

    q = deque()
    q.append(start)

    while q:
        i = q.popleft()
        for on in ons(i):
            if all(below in visited for below in belows(on)):
                if on not in visited:
                    visited.add(on)
                    q.append(on)

    return len(visited) - 1


cubes = []
for line in lines:
    x0, y0, z0, x1, y1, z1 = ints(line)
    cube = (Range(x0, x1 + 1), Range(y0, y1 + 1), Range(z0, z1 + 1))
    cubes.append(cube)
cubes.sort(key=lambda c: c[2].start)

# Drop all cubes
for i in range(len(cubes)):
    drop(i)

puzzle.answer_a = sum(1 for i in range(len(cubes)) if amt_would_drop(i) == 0)
puzzle.answer_b = sum(amt_would_drop(i) for i in range(len(cubes)))
