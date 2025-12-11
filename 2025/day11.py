# 11   00:02:25   00:05:38
# Today was unreasonably easy?

# pyright: basic

import functools
from collections import defaultdict

from aocd.models import Puzzle

puzzle = Puzzle(2025, int("11"))


def solve_a(input: str):
    g = defaultdict(list)

    for line in input.splitlines():
        start, to = line.split(": ")
        tos = to.split(" ")
        g[start] += tos

    @functools.cache
    def paths(a: str, b: str) -> int:
        if a == b:
            return 1

        return sum(paths(to, b) for to in g[a])

    return paths("you", "out")


def solve_b(input: str):
    g = defaultdict(list)

    for line in input.splitlines():
        start, to = line.split(": ")
        tos = to.split(" ")
        g[start] += tos

    @functools.cache
    def paths(a: str, b: str, x: bool, y: bool) -> int:
        x |= a == "dac"
        y |= a == "fft"

        if a == b and x and y:
            return 1

        return sum(paths(to, b, x, y) for to in g[a])

    return paths("svr", "out", False, False)


puzzle.answer_a = solve_a(puzzle.input_data)
puzzle.answer_b = solve_b(puzzle.input_data)
