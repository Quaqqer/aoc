# 13   00:30:46  2241      0   00:33:07  1177      0
# fun problem, struggled a bit with tuple slicing to equate sizes

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("13"))
data = puzzle.input_data
grids = [g.splitlines() for g in data.split("\n\n")]


def reflection(l: Sequence[Sequence[str]], p2: bool) -> int:
    for i in range(1, len(l)):
        diffs = sum(
            1 for a, b in zip(l[:i][::-1], l[i:]) for ac, bc in zip(a, b) if ac != bc
        )
        if (p2 and diffs == 1) or (not p2 and diffs == 0):
            return i
    return 0


def solve(grids: list[list[str]], p2: bool) -> int:
    return sum(reflection(g, p2) * 100 + reflection(transpose(g), p2) for g in grids)


puzzle.answer_a = solve(grids, False)
puzzle.answer_b = solve(grids, True)
