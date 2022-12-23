# 23   01:19:19   2165      0   01:22:45   1977      0
# Pretty slow today, whatever

from collections import defaultdict, deque

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("23"))
id = puzzle.input_data


lines = id.splitlines()

w, h = len(lines[0]), len(lines)

map = set()
for y, row in enumerate(lines):
    for x, cell in enumerate(row):
        if cell == "#":
            map.add((x, y))


def ni(pos):
    x, y = pos
    return 0 <= x and x < w and 0 <= y and y < h


def next_map(map, s):
    next = set()

    proposals = defaultdict(list)
    stationary = set()

    for (ex, ey) in map:
        if set(lib.grid.neighbours8((ex, ey))) & map:
            checks = deque(
                [
                    ({(ex, ey - 1), (ex + 1, ey - 1), (ex - 1, ey - 1)}, (ex, ey - 1)),
                    ({(ex, ey + 1), (ex + 1, ey + 1), (ex - 1, ey + 1)}, (ex, ey + 1)),
                    ({(ex - 1, ey), (ex - 1, ey + 1), (ex - 1, ey - 1)}, (ex - 1, ey)),
                    ({(ex + 1, ey), (ex + 1, ey + 1), (ex + 1, ey - 1)}, (ex + 1, ey)),
                ]
            )
            checks.rotate(-s)
            for checkset, pos in checks:
                if not (checkset & map):
                    proposals[pos].append(((ex, ey)))
                    break
            else:
                stationary.add((ex, ey))
        else:
            stationary.add((ex, ey))

    for (px, py), movers in proposals.items():
        match movers:
            case [(ex, ey)]:
                next.add((px, py))
            case movers:
                next.update(movers)

    next.update(stationary)

    return next, (s + 1) % 4


s = 0
for _ in range(10):
    map, s = next_map(map, s)

xs, ys = lib.unzip(map)
rw = max(xs) - min(xs) + 1
rh = max(ys) - min(ys) + 1
ans = rw * rh - len(map)

puzzle.answer_a = ans

i = 10
while True:
    nmap, ns = next_map(map, s)
    i += 1
    if nmap == map:
        break
    map, s = nmap, ns
puzzle.answer_b = i
