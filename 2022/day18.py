# 18   00:04:41    380      0   00:19:28    402      0
# nice times today

from collections import deque

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("18"))
id = puzzle.input_data


cubes = set((x, y, z) for x, y, z in map(lib.all_ints, id.splitlines()))

neighs = [
    (1, 0, 0),
    (-1, 0, 0),
    (0, 1, 0),
    (0, -1, 0),
    (0, 0, 1),
    (0, 0, -1),
]

s = 0
for cube in cubes:
    for d in neighs:
        neigh = lib.tup_add(cube, d)
        if neigh not in cubes:
            s += 1
print(s)


puzzle.answer_a = s


surrounded = set()
free = set()


xs, ys, zs = lib.unzip(cubes)
print(cubes)
print(min(xs), max(xs), min(ys), max(ys), min(zs), max(zs))


def is_surface(c) -> bool:
    if c in surrounded:
        return False
    if c in free:
        return True

    q = deque()
    q.append(c)
    visited = {c}

    i = 0
    while q:
        cube = q.popleft()

        if i > 10000:
            free.update(visited)
            return True

        for d in neighs:
            neigh = lib.tup_add(cube, d)
            if neigh not in cubes and neigh not in visited:
                q.append(neigh)
                visited.add(neigh)

        i += 1

    surrounded.update(visited)
    return False


s = 0
for cube in cubes:
    for d in neighs:
        c = lib.tup_add(cube, d)
        if c not in cubes and is_surface(c):
            s += 1

print(s)
puzzle.answer_b = s
