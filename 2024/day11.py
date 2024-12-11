# 11   00:03:14   264      0   00:07:09   137      0
# Pretty fun and easy day today. Classic DP problem. My initial solution was
# just to do it the naive way of creating a larger and larger list. That is way
# too slow for part 2.
#
# Now I can go to sleep again.

import functools

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2024, int("11"))

data = puzzle.input_data


@functools.cache
def blink(v: int, n: int) -> int:
    if n == 0:
        return 1

    sv = str(v)
    if v == 0:
        return blink(1, n - 1)
    elif len(sv) % 2 == 0:
        return blink(int(sv[: len(sv) // 2]), n - 1) + blink(
            int(sv[len(sv) // 2 :]), n - 1
        )
    else:
        return blink(v * 2024, n - 1)


l = ints(data)
puzzle.answer_a = sum(blink(v, 25) for v in l)
puzzle.answer_b = sum(blink(v, 75) for v in l)
