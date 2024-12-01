#  1   00:01:21    82     19   00:06:50  1502      0
# Das pretti gud, although I got a bit confused in part 2.

from collections import Counter

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2024, int("01"))
data = puzzle.input_data
lines = data.splitlines()

as_, bs = tuple(zip(*(ints(line) for line in lines)))
puzzle.answer_a = sum(abs(b - a) for a, b in zip(sorted(as_), sorted(bs)))

bc = Counter(bs)
puzzle.answer_b = sum(a * bc[a] for a in as_ if a in bc)
