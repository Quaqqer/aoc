# 25   00:06:31   281      0   00:06:37   241      0
# Merry christmas!

from aocd.models import Puzzle
from util import Grid

puzzle = Puzzle(2024, int("25"))

data = puzzle.input_data

locks = []
keys = []

for chunk in data.split("\n\n"):
    chunk = Grid.from_lines(chunk)
    heights = [sum(v == "#" for v in chunk[i, :]) for i in range(chunk.cols)]
    if all(v == "#" for v in chunk[:, -1]):
        keys.append(heights)
    else:
        locks.append(heights)

s = 0
for lock in locks:
    for key in keys:
        if all(a + b <= 7 for a, b in zip(lock, key)):
            s += 1

puzzle.answer_a = s
