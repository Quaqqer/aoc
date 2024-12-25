# 25   00:06:31   281      0   00:06:37   241      0
# Merry christmas!

from aocd.models import Puzzle
from util import Grid

puzzle = Puzzle(2024, int("25"))

data = puzzle.input_data

locks: list[tuple[int, ...]] = []
keys: list[tuple[int, ...]] = []

for g in map(Grid.from_lines, data.split("\n\n")):
    heights = tuple(sum(v == "#" for v in g[i, :]) for i in range(g.cols))

    if all(v == "#" for v in g[:, -1]):
        keys.append(heights)
    else:
        locks.append(heights)

puzzle.answer_a = sum(
    all(a + b <= 7 for a, b in zip(lock, key)) for lock in locks for key in keys
)
