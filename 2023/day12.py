#  12   00:30:20  1912      0   00:34:53   469      0
# pretty happy with my time on part 2 :)

import functools

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("12"))
data = puzzle.input_data
lines = data.splitlines()


def solve(lines: list[str]) -> int:
    s = 0

    for line in lines:
        a, gs_ = line.split(" ")
        gs = ints(gs_)

        @functools.cache
        def damaged(i: int, g: int) -> int:
            if g >= len(gs):
                return 1 if all(c != "#" for c in a[i:]) else 0

            s = 0
            for j in range(i, len(a)):
                ss = a[j : j + gs[g]]
                if (
                    len(ss) == gs[g]
                    and all(c != "." for c in ss)
                    and (len(a) <= j + gs[g] or a[j + gs[g]] != "#")
                ):
                    s += damaged(j + gs[g] + 1, g + 1)

                if a[j] == "#":
                    break

            return s

        s += damaged(0, 0)

    return s


b_lines = []
for line in lines:
    a, gs_ = line.split(" ")
    b_lines.append("?".join(5 * [a]) + " " + ",".join(5 * [gs_]))

puzzle.answer_a = solve(lines)
puzzle.answer_b = solve(b_lines)
