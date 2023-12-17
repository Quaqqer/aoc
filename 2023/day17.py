# 17   00:13:04    89     12   00:51:19   934      0
# First time in top 100!!! 🎉🎉🎉
# Choked a bit on part 2, I had two bugs:
# - I didn't check the path downwards
# - I didn't count when I turned as a step in that direction

from heapq import heappop as pop
from heapq import heappush as push

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("17"))
data = puzzle.input_data
g = Grid.from_lines(data).map(int)


def search(g: Grid[int], p2: bool) -> int:
    visited: set[tuple[int, int, int, int]] = {(0, 0, 0, 0)}
    queue: list[tuple[int, int, int, int, int]] = [(0, 0, 0, 0, 0), (0, 0, 0, 1, 0)]

    while queue:
        cost, x, y, d, a = pop(queue)

        if (x, y) == (g.cols - 1, g.rows - 1):
            if p2 and a < 4:
                continue
            return cost

        for dd in [-1, 0, 1]:
            if p2:
                if dd != 0 and a < 4:
                    continue

                if dd == 0 and a >= 10:
                    continue
            else:
                if dd == 0 and a >= 3:
                    continue

            nd = (d + dd) % 4

            match nd:
                case 0:
                    nx = x + 1
                    ny = y
                case 1:
                    nx = x
                    ny = y + 1
                case 2:
                    nx = x - 1
                    ny = y
                case 3:
                    nx = x
                    ny = y - 1
                case _:
                    assert False

            aa = a + 1 if dd == 0 else 1

            visit = nx, ny, nd, aa
            if visit in visited or (nx, ny) not in g:
                continue

            visited.add(visit)
            push(queue, (cost + g[nx, ny], nx, ny, nd, aa))

    assert False


puzzle.answer_a = search(g, False)
puzzle.answer_b = search(g, True)
