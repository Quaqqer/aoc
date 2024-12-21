# 21   00:46:18   174      0   03:40:47  1551      0
# Catastrophe, either I'm just dumb or sleeping ~6 hours per day in december is
# not enough. It's probably the former though.

import functools

from aocd.models import Puzzle
from util import Grid

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

    xdir = "<" if dx < 0 else ">"
    ydir = "^" if dy < 0 else "v"

    possibles = []
    # Move x first, then y
    if pad[x + dx, y] != " ":
        path = xdir * abs(dx) + ydir * abs(dy) + "A"
        c = sum(cost(a, b, i + 1, n_dirpads) for a, b in zip("A" + path, path))
        possibles.append(c)

    # Move y first, then x
    if pad[x, y + dy] != " ":
        path = ydir * abs(dy) + xdir * abs(dx) + "A"
        c = sum(cost(a, b, i + 1, n_dirpads) for a, b in zip("A" + path, path))
        possibles.append(c)

    return min(possibles)


def solve(n: int):
    s = 0
    for line in lines:
        move_cost = sum(cost(a, b, 0, n) for a, b in zip("A" + line, line))
        d = int("".join(c for c in line if c.isdigit()))
        s += d * move_cost
    return s


puzzle.answer_a = solve(2)
puzzle.answer_b = solve(25)
