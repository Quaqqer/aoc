# 16   00:25:05  1128      0   00:28:20   809      0
# Eric was sneaky today with the mirror in the start! It helped to know for
# part 2 at least I guess

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("16"))
data = puzzle.input_data
g = Grid.from_lines(data)


def search(bx: int, by: int, bdx: int, bdy: int):
    """Search in grid of mirrors and splitters for energized tiles"""

    visited = set()
    beams = [(bx - bdx, by - bdy, bdx, bdy)]

    def add(x, y, dx, dy):
        v = x, y, dx, dy
        if v not in visited:
            visited.add(v)
            beams.append(v)

    while beams:
        x, y, dx, dy = beams.pop()
        nx, ny = x + dx, y + dy

        if (nx, ny) not in g:
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

    return len(set((x, y) for x, y, _, _ in visited))


puzzle.answer_a = search(0, 0, 1, 0)
puzzle.answer_b = max(
    *(search(x, 0, 0, 1) for x in range(g.cols)),
    *(search(x, g.rows - 1, 0, -1) for x in range(g.cols)),
    *(search(0, y, 1, 0) for y in range(g.rows)),
    *(search(g.cols - 1, y, -1, 0) for y in range(g.rows)),
)
