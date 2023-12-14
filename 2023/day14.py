# 14   00:05:32   260      0   00:34:57   898      0
# pretty good today, was a bit slow to fix part 2.

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("14"))
data = puzzle.input_data
lines = data.splitlines()


def move(g: Grid[str], dx: int, dy: int):
    if dx != 0:
        x = 0
        if dx > 0:
            x = g.cols - 1

        while 0 <= x < g.cols:
            for y in range(g.rows):
                if g[x, y] == "O":
                    xx = x + dx
                    while 0 <= xx < g.cols and g[xx, y] == ".":
                        g[xx, y] = "O"
                        g[xx - dx, y] = "."
                        xx += dx
            x -= dx
    else:
        assert dy != 0
        y = 0
        if dy > 0:
            y = g.rows - 1

        while 0 <= y < g.rows:
            for x in range(g.cols):
                if g[x, y] == "O":
                    yy = y + dy
                    while 0 <= yy < g.rows and g[x, yy] == ".":
                        g[x, yy] = "O"
                        g[x, yy - dy] = "."
                        yy += dy
            y -= dy

    return g


def freeze(g: Grid) -> tuple[tuple[str, ...], ...]:
    return tuple(tuple(r for r in rs) for rs in g._grid)


def count(g: Grid[str]) -> int:
    s = 0
    for y in range(g.rows):
        for x in range(g.cols):
            if g[x, y] == "O":
                s += g.rows - y
    return s


def move_b(g: Grid[str]) -> Grid[str]:
    visits: dict[tuple[tuple[str, ...], ...], int] = {freeze(g): 0}
    i = 0
    while i < 1000000000:
        for dx, dy in [(0, -1), (-1, 0), (0, 1), (1, 0)]:
            move(g, dx, dy)
        i += 1

        frozen = freeze(g)

        if frozen in visits:
            length = i - visits[frozen]
            count = (10**9 - i) // length
            i += length * count
        visits[frozen] = i

    return g


g = Grid.from_lines(data)
puzzle.answer_a = count(move(g.copy(), 0, -1))
puzzle.answer_b = count(move_b(g.copy()))
