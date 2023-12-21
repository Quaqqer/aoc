# 21   00:08:35   559      0   04:10:57  1997      0
# Quite a long delta time today 😐
# I don't like problems that have very specific input, because I want to solve
# the problems generally, but I guess the input is part of the puzzle. I don't
# quite understand why we can use a quadratic to solve it, but I just wanted to
# finish the day. At least I learned how to do quadratic interpolation, that's
# pretty cool.
#
# I initially tried to calculate the diamond by calculating squares fully
# inside + squares on the edge aligned with the start + squares on the edge.
# The problem I had was that it was hard to validate since the example did not
# follow the very specific input that was provided.

from collections import deque

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("21"))
data = puzzle.input_data
lines = data.splitlines()

g = Grid.from_lines(data)
s = g.find(lambda _, v: v == "S").__next__()
g[s] = "."


def endpoints(g: Grid, start: Coord, limit: int, even: bool) -> int:
    visited = {start}

    q = deque()
    q.append((start, 0))

    ans = 0
    while q:
        pos, dist = q.popleft()

        if even:
            if dist % 2 == 0:
                ans += 1
        else:
            if dist % 2 == 1:
                ans += 1

        for neigh in g.neighbours4(pos, inside=False):
            nx, ny = neigh
            if neigh not in visited and g[nx % g.cols, ny % g.rows] != "#":
                visited.add(neigh)

                if dist < limit:
                    q.append((neigh, dist + 1))

    return ans


def interpolate_b(x: int) -> int:
    x0 = 262 * 0 + 65
    x1 = 262 * 1 + 65
    x2 = 262 * 2 + 65
    y0 = endpoints(g, s, x0, False)
    y1 = endpoints(g, s, x1, False)
    y2 = endpoints(g, s, x2, False)

    def l0(x: int) -> float:
        return ((x - x1) * (x - x2)) / ((x0 - x1) * (x0 - x2))

    def l1(x: int) -> float:
        return ((x - x0) * (x - x2)) / ((x1 - x0) * (x1 - x2))

    def l2(x: int) -> float:
        return ((x - x0) * (x - x1)) / ((x2 - x0) * (x2 - x1))

    return round(y0 * l0(x) + y1 * l1(x) + y2 * l2(x))


puzzle.answer_a = endpoints(g, s, 64, True)
puzzle.answer_b = interpolate_b(26501365)
