#  10   02:07:35  8107      0   03:31:46  4800      0
# Today was rough, slept < 4 hours. Had i slept 4 hours more maybe I could have
# spent 2 hours less on the problem. Don't sleep on sleep kids.

from collections import defaultdict
from typing import Literal

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("10"))
data = puzzle.input_data
grid = Grid.from_lines(data)

g = defaultdict(list)

start = None


def connect(
    coord: Coord,
    dir: Literal["left"] | Literal["right"] | Literal["up"] | Literal["down"],
):
    dx, dy = 0, 0
    match dir:
        case "left":
            dx, dy = -1, 0
        case "right":
            dx, dy = 1, 0
        case "up":
            dx, dy = 0, -1
        case "down":
            dx, dy = 0, 1
        case _:
            assert False
    x, y = coord
    n = x + dx, y + dy

    if grid.is_outside(*n):
        return False

    ok = False
    match dir:
        case "left":
            ok = grid[n] in "LF-"
        case "right":
            ok = grid[n] in "7J-"
        case "up":
            ok = grid[n] in "7F|"
        case "down":
            ok = grid[n] in "JL|"
        case _:
            assert False

    if ok:
        g[n].append(coord)
        g[coord].append(n)

    return ok


for c in grid.coords():
    v = grid[c]

    match v:
        case "|":
            connect(c, "down")
            connect(c, "up")
        case "-":
            connect(c, "left")
            connect(c, "right")
        case "L":
            connect(c, "up")
            connect(c, "right")
        case "J":
            connect(c, "up")
            connect(c, "left")
        case "7":
            connect(c, "left")
            connect(c, "down")
        case "F":
            connect(c, "right")
            connect(c, "down")
        case ".":
            pass
        case "S":
            start = c
            up = connect(c, "up")
            down = connect(c, "down")
            left = connect(c, "left")
            right = connect(c, "right")

            match up, down, left, right:
                case True, True, False, False:
                    grid[c] = "|"
                case False, False, True, True:
                    grid[c] = "-"
                case True, False, True, False:
                    grid[c] = "J"
                case True, False, False, True:
                    grid[c] = "L"
                case False, True, True, False:
                    grid[c] = "7"
                case False, True, False, True:
                    grid[c] = "F"


assert start is not None


loop: set[Coord] = set()


def search():
    i = 0
    q: list[tuple[int, tuple[int, int]]] = [(0, start)]

    visited = {start}

    best = 0
    while len(q) > i:
        dist, coord = q[i]
        if best < dist:
            best = dist

        for e in g[coord]:
            if e not in visited:
                visited.add(e)
                q.append((dist + 1, e))

        i += 1

    global loop
    loop = visited
    return best


rows2 = grid.rows * 3
cols2 = grid.cols * 3
grid2 = Grid(cols2, rows2, [["." for _ in range(cols2)] for _ in range(rows2)])

for x, y in grid.coords():
    s = None
    match grid[x, y]:
        case "|":
            s = [[".", "|", "."], [".", "|", "."], [".", "|", "."]]
        case "-":
            s = [[".", ".", "."], ["-", "-", "-"], [".", ".", "."]]
        case "L":
            s = [[".", "|", "."], [".", "L", "-"], [".", ".", "."]]
        case "J":
            s = [[".", "|", "."], ["-", "J", "."], [".", ".", "."]]
        case "7":
            s = [[".", ".", "."], ["-", "7", "."], [".", "|", "."]]
        case "F":
            s = [[".", ".", "."], [".", "F", "-"], [".", "|", "."]]

    if s is not None:
        for dx in range(3):
            for dy in range(3):
                grid2[x * 3 + dx, y * 3 + dy] = s[dy][dx]


for coord in grid2.coords():
    if grid2[coord] != ".":
        continue

    visited = {coord}
    taint = False

    q = [coord]
    while q:
        c = q.pop()
        cx, cy = c

        if cx == 0 or cx == grid2.cols - 1 or cy == 0 or cy == grid2.rows - 1:
            taint = True

        for n in grid2.neighbours4(c):
            if grid2[n] == "." and n not in visited:
                q.append(n)
                visited.add(n)

    for c in visited:
        grid2[c] = "O" if taint else "I"

puzzle.answer_a = search()

insides = 0
insides = sum(
    1
    for x, y in grid.coords()
    if (x, y) not in loop
    and not any(
        grid2[3 * x + dx, 3 * y + dy] == "O" for dx in range(3) for dy in range(3)
    )
)


puzzle.answer_b = insides
