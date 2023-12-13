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

        if p2:
            # If there is only one differing character, we found a mirror
            diffs = sum(
                1 for j in range(len(a)) for ac, bc in zip(a[j], b[j]) if ac != bc
            )
            if diffs == 1:
                return i
        else:
            # If they are equal we found a mirror
            if a == b:
                return i


asum = 0
bsum = 0
for g in grids:
    for p2 in [False, True]:
        hr = reflection(g, p2)
        vr = reflection(transpose(g), p2)

        if hr is not None:
            s = 100 * hr
        else:
            assert vr is not None
            s = vr

        if p2:
            bsum += s
        else:
            asum += s

puzzle.answer_a = asum
puzzle.answer_b = bsum
