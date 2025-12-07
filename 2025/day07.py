# 7   00:04:31   00:08:53
# Good times today! Grids are always fun! 😄

# pyright: basic

import functools

from aocd.models import Puzzle
from util import Grid

puzzle = Puzzle(2025, int("07"))


def solve_a(input: str):
    g = Grid.from_lines(input)

    (s,) = g.find_value("S")

    splits = 0
    q = [s]

    while q:
        x, y = q.pop()

        g[x, y] = "|"

        ny = y + 1
        np = x, ny

        if np in g and g[np] == ".":
            q += [np]
        elif np in g and g[np] == "^":
            splits += 1
            q += [(x - 1, ny), (x + 1, ny)]
    return splits


@functools.cache
def splits(g: Grid[str], p: tuple[int, int]) -> int:
    x, y = p

    if p not in g:
        return 1
    elif g[p] == "^":
        return splits(g, (x + 1, y + 1)) + splits(g, (x - 1, y + 1))
    else:
        return splits(g, (x, y + 1))


def solve_b(input: str):
    g = Grid.from_lines(input)
    (s,) = g.find_value("S")
    return splits(g, s)


puzzle.answer_a = solve_a(puzzle.input_data)
puzzle.answer_b = solve_b(puzzle.input_data)
