# 9   00:03:14   01:08:00
# Started looking at line segment intersection, way too complicated for a grid
# problem. 😅

# pyright: basic

from __future__ import annotations

from aocd.models import Puzzle
from util import Vec2, ints

puzzle = Puzzle(2025, int("09"))


def area(v1, v2):
    return (abs(v1.x - v2.x) + 1) * (abs(v1.y - v2.y) + 1)


def ok(v1: Vec2[int], v2: Vec2[int], reds: list[Vec2[int]], part2: bool) -> bool:
    if not part2:
        return True

    miny = min(v1.y, v2.y)
    maxy = max(v1.y, v2.y)
    minx = min(v1.x, v2.x)
    maxx = max(v1.x, v2.x)

    for a, b in zip(reds, reds[1:] + [reds[0]]):
        above = a.y >= maxy and b.y >= maxy
        below = a.y <= miny and b.y <= miny
        left = a.x <= minx and b.x <= minx
        right = a.x >= maxx and b.x >= maxx

        if not (above or below or left or right):
            return False

    return True


def solve(input: str, part2: bool):
    red: list[Vec2] = []
    for line in input.splitlines():
        x, y = ints(line)
        red.append(Vec2(x, y))

    return max(
        area(v1, v2) for v1 in red for v2 in red if v1 != v2 and ok(v1, v2, red, part2)
    )


puzzle.answer_a = solve(puzzle.input_data, False)
puzzle.answer_b = solve(puzzle.input_data, True)
