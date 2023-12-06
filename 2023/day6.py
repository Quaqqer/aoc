#   6   00:10:31  2929      0   00:11:52  1743      0
# meh

from math import prod

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("06"))
times, dists = [ints(l.split(":")[1]) for l in puzzle.input_data.splitlines()]


def best_time(time, dist) -> int:
    return sum(1 for v in (t * (time - t) for t in range(0, time + 1)) if v > dist)


puzzle.answer_a = prod(best_time(time, dist) for time, dist in zip(times, dists))

puzzle.answer_b = best_time(
    int("".join(str(s) for s in times)), int("".join(str(s) for s in dists))
)
