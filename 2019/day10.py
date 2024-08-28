import math
from collections import defaultdict

from aocd.models import Puzzle

puzzle = Puzzle(2019, int("10"))
data = puzzle.input_data
lines = data.splitlines()

# Get a set of asteroid locations
asteroids = set()
for y in range(len(lines)):
    for x in range(len(lines[y])):
        if lines[y][x] == "#":
            asteroids.add((x, y))

# Find the best scanner location by counting different angles to other
# asteroids, floating point may not be optimal. Could use a set of points and
# use linear algebra to check for colinear points instead, this would avoid
# floating point but it seems to work fine.
visible, scanner = max(
    (
        len(
            {
                math.atan2(oy - ay, ox - ax)
                for ox, oy in asteroids
                if not (ox == ax and oy == ay)
            }
        ),
        (ax, ay),
    )
    for ax, ay in asteroids
)

puzzle.answer_a = visible


# :grimacing:
def mod(a, m):
    return ((a % m) + m) % m


# Create a list of asteroids for each angle
#
# For every other asteroid, get the angle
scanner_angles: dict[float, list[tuple[int, int]]] = defaultdict(list)
ax, ay = scanner
for ox, oy in asteroids:
    if ax == ox and ay == oy:
        continue

    scanner_angles[math.atan2(oy - ay, ox - ax)].append((ox, oy))

# Sort asteroid list for each angle by the distance from our scanner, reverse
# so we can pop closest
for v in scanner_angles.values():
    v.sort(key=lambda pos: math.hypot(pos[0] - ax, pos[1] - ay), reverse=True)

# Use sorted list, sorted by order of vaporization order, so offset by 90°
to_vaporize = sorted(
    ((angle, asteroids) for angle, asteroids in scanner_angles.items()),
    key=lambda v: mod(v[0] + math.pi / 2, math.pi * 2),
)


def find_ans_b() -> int:
    # Go through lists until we vaporized 200 or no more exist
    d = 0
    while any(len(vaporization_list) != 0 for vaporization_list in to_vaporize):
        for _, asteroids in to_vaporize:
            if d == 199:
                x, y = asteroids[-1]
                return x * 100 + y
            if asteroids:
                asteroids.pop()
                d += 1
    raise Exception("No more asteroids to vaporize")


puzzle.answer_b = find_ans_b()
