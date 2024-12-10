# 10   00:08:32   797      0   00:08:49   439      0
# I accidentally implemented part 2 in part 1, so to solve part 2 I just had to
# press the undo key.

from aocd.models import Puzzle
from util import Grid

puzzle = Puzzle(2024, int("10"))

data = puzzle.input_data

g = Grid.from_lines(data).map(int)


def n_tops(pos: tuple[int, int]) -> set[tuple[int, int]]:
    height = g[pos]
    if height == 9:
        return {pos}
    s = set()
    for neigh in g.neighbours4(pos):
        if g[neigh] == height + 1:
            s |= n_tops(neigh)
    return s


def n_trails(pos: tuple[int, int]) -> int:
    height = g[pos]
    if height == 9:
        return 1
    s = 0
    for neigh in g.neighbours4(pos):
        if g[neigh] == height + 1:
            s += n_trails(neigh)
    return s


puzzle.answer_a = sum(len(n_tops((x, y))) for x, y in g.coords() if g[x, y] == 0)
puzzle.answer_b = sum(n_trails((x, y)) for x, y in g.coords() if g[x, y] == 0)
