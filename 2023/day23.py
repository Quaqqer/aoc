# 23   00:10:20   150      0   00:47:28   182      0
# Fast solution => good day, hooray! 🎉

from heapq import heappop as pop
from heapq import heappush as push

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("23"))
data = puzzle.input_data
lines = data.splitlines()

g = Grid.from_lines(data)

s = (1, 0)
e = (g.cols - 2, g.rows - 1)


def intersections(
    start: Coord,
    p2: bool,
    p: "Coord | None" = None,
    visited: set[Coord] | None = None,
    dist: int = 0,
) -> dict[Coord, int]:
    if p is None:
        p = start

    if visited is None:
        visited = {p}

    ns = [n for n in g.neighbours4(p) if g[n] != "#"]

    if p != start and (len(ns) > 2 or p == s or p == e):
        return {p: dist}

    ins = {}
    for n in ns:
        if n not in visited:
            if not p2:
                nx, ny = n
                x, y = p
                dx, dy = nx - x, ny - y
                if g[n] == ">" and dx != 1 or g[n] == "v" and dy != 1:
                    continue
            ins |= intersections(start, p2, n, visited | {n}, dist + 1)

    return ins


def find_all_intersections(g: Grid[str], p2: bool) -> dict[Coord, dict[Coord, int]]:
    return {
        p: intersections(p, p2)
        for p in g
        if g[p] != "#"
        if p == s or len([n for n in g.neighbours4(p) if g[n] != "#"]) > 2
    }


def max_path(g: Grid[str], p2: bool) -> int:
    inns = find_all_intersections(g, p2)

    anss = []

    q: list[tuple[int, Coord, set[Coord]]] = [(0, s, {s})]
    while q:
        dist, coord, visited = pop(q)

        if coord == e:
            anss.append(-dist)
        else:
            for neigh, d in inns[coord].items():
                if neigh not in visited:
                    push(q, (dist - d, neigh, visited | {neigh}))

    return max(anss)


puzzle.answer_a = max_path(g, False)
puzzle.answer_b = max_path(g, True)
