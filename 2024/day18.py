# 18   00:11:15   947      0   00:18:26  1059      0
# Had a off by one bug on part 2 😬

import bisect
from typing import cast

from aocd.models import Puzzle
from util import Heap, Vec2, ints

puzzle = Puzzle(2024, int("18"))
data = puzzle.input_data

bytes = [Vec2(*ints(line)) for line in data.splitlines()]
start = Vec2(0, 0)
end = Vec2(70, 70)


def search(start: Vec2[int], end: Vec2[int], fallen: set[Vec2[int]]) -> int | None:
    start = Vec2(0, 0)
    q = Heap([(0, 0, start)])

    visited = set()
    while q:
        h, cost, pos = q.pop()

        if pos == end:
            return cost

        if pos in visited:
            continue
        visited.add(pos)

        for d in Vec2.delta_4():
            n = pos + d
            if not (0 <= n.x <= end.y and 0 <= n.y <= end.y):
                continue
            if n in fallen:
                continue
            q.push(((n - end).manhattan() + cost + 1, cost + 1, n))


def solve_b():
    i = bisect.bisect_left(
        range(len(data.splitlines())),
        True,
        key=lambda i: search(start, end, set(bytes[: i + 1])) is None,
    )
    x, y = bytes[i]
    return f"{x},{y}"


puzzle.answer_a = cast(int, search(start, end, set(bytes[:1024])))
puzzle.answer_b = solve_b()
