import functools
from collections import defaultdict
from heapq import heappop, heappush

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("24"))
id = puzzle.input_data

lines = id.splitlines()
width = len(lines[0]) - 2
height = len(lines) - 2


def mod(a, b):
    return ((a % b) + b) % b


def move_blizzard(pos, b):
    x, y = pos
    match b:
        case "v":
            return x, mod(y + 1, height)
        case "^":
            return x, mod(y - 1, height)
        case "<":
            return mod(x - 1, width), y
        case ">":
            return mod(x + 1, width), y


initial_blizzards = defaultdict(list)
for t0_ in range(width):
    for y in range(height):
        cell = lines[y + 1][t0_ + 1]

        if cell in {"v", "^", "<", ">"}:
            initial_blizzards[(t0_, y)].append(cell)


@functools.cache
def blizzards(t):
    if t == 0:
        return initial_blizzards

    prev = blizzards(t - 1)

    next = defaultdict(list)

    for pos, blizs in prev.items():
        for bliz in blizs:
            next[move_blizzard(pos, bliz)].append(bliz)

    return next


def heuristic(pos, time, to_):
    return lib.grid.manhattan(pos, to_) + time


def print_map(t, p):
    d = {}

    map = blizzards(t)
    for pos, blizs in map.items():
        if len(blizs) == 1:
            d[pos] = blizs[0]
        elif len(blizs) > 1:
            d[pos] = len(blizs)

    if p is not None:
        assert p not in d
        d[p] = "E"

    lib.grid.print_set_grid(d, min_x=0, min_y=0, max_x=width, max_y=height)


def search(from_, to_, time_):
    pos = from_
    q = [(heuristic(pos, time_, to_), pos, time_)]
    visited = set()

    while q:
        _, pos, time = heappop(q)

        blizs = blizzards(time + 1)

        if pos == to_:
            return time

        for neigh in lib.grid.neighbours4(pos) + [pos]:
            nx, ny = neigh
            if (0 <= nx < width and 0 <= ny < height or neigh == from_) and (
                neigh,
                time,
            ) not in visited:
                visited.add((neigh, time))

                if not blizs[neigh]:
                    heappush(q, (heuristic(neigh, time + 1, to_), neigh, time + 1))


t0_ = search((0, 0), (width - 1, height - 1), 1)
assert t0_ is not None
t0 = t0_ + 1
puzzle.answer_a = t0

while True:
    if (s := search((width - 1, height - 1), (0, 0), t0)) is not None:
        t1 = s + 1
        break
    else:
        t0 += 1
while True:
    if (s := search((0, 0), (width - 1, height - 1), t1)) is not None:
        t2 = s + 1
        break
    else:
        t1 += 1
puzzle.answer_b = t2
