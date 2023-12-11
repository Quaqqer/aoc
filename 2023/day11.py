#  11   00:06:32   121      0   00:23:46  1384      0
# finally some good fucking food, first time in top 200

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("11"))
data = puzzle.input_data

lines = data.splitlines()
cols = list(zip(*lines))

empty_lines = {i for i, line in enumerate(lines) if all(c == "." for c in line)}
empty_cols = {i for i, col in enumerate(cols) if all(c == "." for c in col)}
galaxies = [
    (y, x) for y in range(len(lines)) for x in range(len(cols)) if lines[y][x] == "#"
]


def gmanhattan(a: Coord, b: Coord, k: int):
    ay, ax = a
    by, bx = b

    return (
        manhattan(a, b)
        + sum(k - 1 for y in range(min(ay, by), max(ay, by) + 1) if y in empty_lines)
        + sum(k - 1 for x in range(min(ax, bx), max(ax, bx) + 1) if x in empty_cols)
    )


puzzle.answer_a = sum(
    gmanhattan(galaxies[i], galaxies[j], 2)
    for i in range(len(galaxies))
    for j in range(i + 1, len(galaxies))
)
puzzle.answer_b = sum(
    gmanhattan(galaxies[i], galaxies[j], 10**6)
    for i in range(len(galaxies))
    for j in range(i + 1, len(galaxies))
)
