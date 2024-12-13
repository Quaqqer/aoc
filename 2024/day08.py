# 8   00:06:55   332      0   00:08:56   183      0
# Pretty good day today, what can I say...
# Vectors and grids were useful today

import itertools
from collections import defaultdict

from aocd.models import Puzzle
from util import Grid, Vec2

puzzle = Puzzle(2024, int("08"))

data = puzzle.input_data

freqs = defaultdict(list)
for y, line in enumerate(data.splitlines()):
    for x, c in enumerate(line):
        if c != ".":
            freqs[c].append(Vec2(x, y))


def solve_a():
    g = Grid.from_lines(data)

    for freq in freqs.values():
        for a, b in itertools.permutations(freq, 2):
            p = b * 2 - a
            if p in g:
                g[p] = "#"
    return len(list(g.find_value("#")))


def solve_b():
    g = Grid.from_lines(data)

    for freq in freqs.values():
        for a, b in itertools.permutations(freq, 2):
            delta = b - a
            pos = a + delta
            while pos in g:
                g[pos] = "#"
                pos += delta
    return len(list(g.find_value("#")))


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
