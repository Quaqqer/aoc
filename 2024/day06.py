# 6   00:09:43  1084      0   00:27:30  1369      0
# Not too shabby I guess

from aocd.models import Puzzle
from util import Grid, Vec2

puzzle = Puzzle(2024, int("06"))
data = puzzle.input_data


def walk(g: Grid, start: Vec2) -> tuple[bool, Grid]:
    pos = start
    dir = Vec2(0, -1)

    visited: set[tuple[Vec2, Vec2]] = set()

    while True:
        g[pos] = "X"

        # Check if we have already visited this position
        if (pos, dir) in visited:
            return True, g
        visited.add((pos, dir))

        # Check if we go out of bounds
        if (pos + dir) not in g:
            return False, g

        # Check if we should rotate
        elif g[pos + dir] == "#":
            dir = dir.rot_r()

        # Move otherwise
        else:
            pos += dir
            g[pos]


def is_infinite_with_barrier(g: Grid, start: Vec2, barrier: Vec2[int]):
    g = g.copy()
    g[barrier] = "#"
    infinite, _ = walk(g, start)
    return infinite


g = Grid.from_lines(data)
[s] = map(Vec2, g.find_value("^"))
g[s] = "."


_, ag = walk(g.copy(), s)
puzzle.answer_a = len(list(ag.find_value("X")))
puzzle.answer_b = sum(is_infinite_with_barrier(g, s, x) for x in ag.find_value("X"))
