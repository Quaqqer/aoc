# 21   00:46:18   174      0   03:40:47  1551      0
# Catastrophe, either I'm just dumb or sleeping ~6 hours per day in december is
# not enough. It's probably the former though.

import functools

from aocd.models import Puzzle
from util import Grid, sign

puzzle = Puzzle(2024, int("21"))

data = puzzle.input_data
lines = data.splitlines()

kpad = Grid.from_lines("789\n456\n123\n 0A")
dpad = Grid.from_lines(" ^A\n<v>")
kpad_pos = {kpad[p]: p for p in kpad.vecs()}
dpad_pos = {dpad[p]: p for p in dpad.vecs()}


@functools.cache
def cost(from_: str, to: str, i: int, n_dirpads: int) -> int:
    if i > n_dirpads:
        # I'm only human after all
        return 1

    pad, poss = (kpad, kpad_pos) if i == 0 else (dpad, dpad_pos)
    x, y = poss[from_]
    dx, dy = poss[to] - poss[from_]

    dxs = {1: ">", -1: "<"}[sign(dx)] * abs(dx) if dx != 0 else ""
    dys = {1: "v", -1: "^"}[sign(dy)] * abs(dy) if dy != 0 else ""

    paths = []

    # Move x first, then y
    if pad.get(x + dx, y, " ") != " ":
        paths += ["A" + dxs + dys + "A"]

    # Move y first, then x
    if pad.get(x, y + dy, " ") != " ":
        paths += ["A" + dys + dxs + "A"]

    return min(
        sum(cost(a, b, i + 1, n_dirpads) for a, b in zip(path, path[1:]))
        for path in paths
    )


def solve(n: int):
    s = 0
    for line in lines:
        move_cost = sum(cost(a, b, 0, n) for a, b in zip("A" + line, line))
        d = int("".join(c for c in line if c.isdigit()))
        s += d * move_cost
    return s


puzzle.answer_a = solve(2)
puzzle.answer_b = solve(25)
