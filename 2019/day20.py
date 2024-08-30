import functools
import itertools
import math
import pprint
import re
from collections import Counter, defaultdict, deque
from copy import copy, deepcopy
from dataclasses import dataclass

import util
from aocd.models import Puzzle
from util import Grid, Heap, Vec2, Vec3

puzzle = Puzzle(2019, int("20"))
data = puzzle.input_data

g = Grid.from_lines(data)

portals: dict[str, list[Vec2[int]]] = defaultdict(list)
for x in range(g.cols):
    for y in range(g.rows):
        pos = Vec2(x, y)

        c = g[pos]

        if not c.isupper():
            continue

        for delta in [Vec2(1, 0), Vec2(0, 1)]:
            pos2 = pos + delta
            if pos2 in g and g[pos2].isupper():
                l = g[pos] + g[pos2]

                if any(g[neigh] == "." for neigh in g.neighbours4(pos.tup)):
                    portals[l].append(pos)
                    g[pos] = "o"
                else:
                    g[pos] = " "

                if any(g[neigh] == "." for neigh in g.neighbours4(pos2.tup)):
                    portals[l].append(pos2)
                    g[pos2] = "o"
                else:
                    g[pos2] = " "
portal_positions: dict[Vec2[int], str] = {}
for portal, poss in portals.items():
    for pos in poss:
        portal_positions[pos] = portal


def find_distances(g: Grid[str], start: Vec2[int]) -> dict[str, int]:
    visited = {start}

    distances: dict[str, int] = {}

    q = deque([(0, start)])

    while q:
        cost, pos = q.popleft()

        for neigh in (pos + delta for delta in Vec2.delta_4()):
            if neigh not in visited and g[neigh] in {".", "o"}:
                if g[neigh] == "o":
                    distances[portal_positions[neigh]] = cost

                    continue

                visited.add(neigh)
                q.append((cost + 1, neigh))

    return distances


distances: dict[str, dict[str, int]] = defaultdict(dict)
for pos, portal in portal_positions.items():
    dists = find_distances(g, pos)
    for dist, cost in dists.items():
        distances[portal][dist] = cost


def dijkstra[T](start: T, end: T, edges: dict[T, dict[T, int]]) -> int:
    q = Heap([(-1, start)])

    visited = set()

    while q:
        cost, pos = q.pop()

        if pos in visited:
            continue
        else:
            visited.add(pos)

        if pos == end:
            return cost

        for portal, portal_cost in edges[pos].items():
            q.push((cost + portal_cost, portal))

    raise Exception()


puzzle.answer_a = dijkstra("AA", "ZZ", distances)
