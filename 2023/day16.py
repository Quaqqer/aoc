# 16   00:25:05  1128      0   00:28:20   809      0
# Eric was sneaky today with the mirror in the start!

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("16"))
data = puzzle.input_data
g = Grid.from_lines(data)


def search(bx: int, by: int, bdx: int, bdy: int):
    """Search in grid of mirrors and splitters for energized tiles

    Because the first tile may be a mirror or splitter, start just outside the grid.
    For instance: `search(-1, 0, 1, 0)` instead of `search(0, 0, 1, 0)`
    """

    visited = set()
    beams = [((bx, by), (bdx, bdy))]

    def add(x, y, dx, dy):
        v = (x, y), (dx, dy)
        if v not in visited:
            visited.add(v)
            beams.append(v)

    while beams:
        pos, dir = beams.pop()
        x, y = pos
        dx, dy = dir

        nx, ny = x + dx, y + dy
        if g.is_outside(nx, ny):
            continue

        match g[nx, ny]:
            case "/":
                match dx, dy:
                    case 1, 0:
                        add(nx, ny, 0, -1)
                    case -1, 0:
                        add(nx, ny, 0, 1)
                    case 0, -1:
                        add(nx, ny, 1, 0)
                    case 0, 1:
                        add(nx, ny, -1, 0)
            case "\\":
                match dx, dy:
                    case 1, 0:
                        add(nx, ny, 0, 1)
                    case -1, 0:
                        add(nx, ny, 0, -1)
                    case 0, -1:
                        add(nx, ny, -1, 0)
                    case 0, 1:
                        add(nx, ny, 1, 0)
            case "|":
                match dx, dy:
                    case (1, 0) | (-1, 0):
                        add(nx, ny, 0, -1)
                        add(nx, ny, 0, 1)
                    case _:
                        add(nx, ny, dx, dy)
            case "-":
                match dx, dy:
                    case (0, 1) | (0, -1):
                        add(nx, ny, -1, 0)
                        add(nx, ny, 1, 0)
                    case _:
                        add(nx, ny, dx, dy)
            case ".":
                add(nx, ny, dx, dy)

    return len(set(pos for pos, _ in visited))


puzzle.answer_a = search(-1, 0, 1, 0)
puzzle.answer_b = max(
    *(search(x, -1, 0, 1) for x in range(g.cols)),
    *(search(x, g.rows, 0, -1) for x in range(g.cols)),
    *(search(-1, y, 1, 0) for y in range(g.rows)),
    *(search(g.cols, y, -1, 0) for y in range(g.rows)),
)
