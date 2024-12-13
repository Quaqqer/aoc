# 2   00:12:24  3909      0   00:23:59  3582      0
# I blame it on the outage

from typing import Sequence

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2024, int("02"))
data = puzzle.input_data
nss = [ints(line) for line in data.splitlines()]


def safe(ns: Sequence[int]):
    increasing = all(a < b for a, b in zip(ns, ns[1:]))
    decreasing = all(a > b for a, b in zip(ns, ns[1:]))
    dist = all(1 <= abs(a - b) <= 3 for a, b in zip(ns, ns[1:]))
    return (increasing or decreasing) and dist


puzzle.answer_a = sum(safe(ns) for ns in nss)
puzzle.answer_b = sum(
    any(safe(ns[:i] + ns[i + 1 :]) for i in range(len(ns))) for ns in nss
)
