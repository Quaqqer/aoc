# 13   00:30:46  2241      0   00:33:07  1177      0
# fun problem, struggled a bit with tuple slicing to equate sizes

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("13"))
data = puzzle.input_data
grids = [g.splitlines() for g in data.split("\n\n")]


def reflection(l: Sequence[Sequence[str]], p2: bool) -> int | None:
    # For every index i [1, len(l) - 1]
    for i in range(1, len(l)):
        # slice a, b such that we get two slices of equal length on the
        # opposite sides of i
        length = min(i, len(l) - i)
        a = tuple(l[i - length : i])
        b = tuple(l[i : i + length])[::-1]

        diffs = sum(1 for j in range(len(a)) for ac, bc in zip(a[j], b[j]) if ac != bc)

        if (p2 and diffs == 1) or (not p2 and diffs == 0):
            return i


def solve(grids: list[list[str]], p2: bool) -> int:
    s = 0
    for g in grids:
        if (hr := reflection(g, p2)) is not None:
            s += 100 * hr
        elif (vr := reflection(transpose(g), p2)) is not None:
            s += vr
    return s


puzzle.answer_a = solve(grids, False)
puzzle.answer_b = solve(grids, True)
