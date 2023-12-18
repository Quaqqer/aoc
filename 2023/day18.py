# 18   00:19:03   703      0   01:16:47  1552      0
# Another slow day! Can't win them all!
#
# My solution is quite a bit like day 10 with the pipes. Simplified it works like this:
#
# I take every position that I visited and create a grid of them.
# If I go 10 steps right from 0 I create three x-positions in the grid: 0, 1,
# and 2, with the widths 1, 8, 1 respectively. If I visit x = 5 later I need to
# make sure to split the point 1 (width 8) into three new points with widths 3,
# 1, 3.
# Then I flood fill to find the outside nodes and sum the width * the height
# for every point in the grid not in the outside.

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("18"))
data = puzzle.input_data


def find_path(data: str, p2: bool) -> list[Coord]:
    """Create the path from the input"""
    x, y = 0, 0

    coords: list[Coord] = [(x, y)]

    for line in data.splitlines():
        dir, amt, color = line.split()

        if p2:
            dir = {"0": "R", "1": "D", "2": "L", "3": "U"}[color[-2]]
            amt = int(color[2:-2], 16)
        else:
            amt = int(amt)

        match dir:
            case "U":
                y -= amt
            case "D":
                y += amt
            case "L":
                x -= amt
            case "R":
                x += amt

        coords.append((x, y))

    return coords


def find_dug(
    path: list[Coord],
) -> tuple[list[int], list[int], set[Coord]]:
    """Follow the path along the grid and create a grid from it.

    The points in the grid have widths and heights from the return value.

    Returns:
        widths, heights, dug points
    """

    xs = sorted(set(x for x, _ in path))
    widths = flatten([[1, x2 - x1 - 1] for x1, x2 in zip(xs, xs[1:])]) + [1]

    ys = sorted(set(y for _, y in path))
    heights = flatten([[1, y2 - y1 - 1] for y1, y2 in zip(ys, ys[1:])]) + [1]

    dug: set[Coord] = set()
    for a, b in zip(path, path[1:]):
        ax, ay = a
        bx, by = b

        if ax != bx:
            yi = ys.index(ay) * 2
            minx = min(ax, bx)
            maxx = max(ax, bx)
            begin = xs.index(minx) * 2
            end = xs.index(maxx) * 2
            for x in range(begin, end + 1):
                dug.add((x, yi))
        else:
            xi = xs.index(ax) * 2
            miny = min(ay, by)
            maxy = max(ay, by)
            begin = ys.index(miny) * 2
            end = ys.index(maxy) * 2

            for y in range(begin, end + 1):
                dug.add((xi, y))

    return widths, heights, dug


def find_outside(dug: set[Coord]) -> set[Coord]:
    """Find the points not contained within the dug path.

    Only finds for x in [minx, maxx] and y in [miny, maxy].
    """
    outside: set[Coord] = set()

    minx = min(x for x, _ in dug)
    maxx = max(x for x, _ in dug)
    miny = min(y for _, y in dug)
    maxy = max(y for _, y in dug)

    def flood(x: int, y: int):
        if (x, y) in dug or (x, y) in outside:
            return

        q = [(x, y)]

        while q:
            x2, y2 = q.pop()
            outside.add((x2, y2))

            for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                x3 = x2 + dx
                y3 = y2 + dy
                if (
                    minx <= x3 <= maxx
                    and miny <= y3 <= maxy
                    and (x3, y3) not in outside
                    and (x3, y3) not in dug
                ):
                    q.append((x3, y3))
                    outside.add((x3, y3))

    for x in range(minx, maxx + 1):
        flood(x, miny)
        flood(x, maxy)

    for y in range(miny, maxy + 1):
        flood(minx, y)
        flood(maxx, y)

    return outside


def solve(p2: bool) -> int:
    """Solve the day"""
    path = find_path(data, p2)
    widths, heights, dug = find_dug(path)
    outside = find_outside(dug)

    dug_amt = sum(
        w * h
        for xi, w in enumerate(widths)
        for yi, h in enumerate(heights)
        if (xi, yi) not in outside
    )

    return dug_amt


puzzle.answer_a = solve(False)
puzzle.answer_b = solve(True)
