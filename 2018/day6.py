from collections import Counter

from aocd.models import Puzzle
from util import Vec2, ints

puzzle = Puzzle(2018, int("06"))
data = puzzle.input_data
lines = data.splitlines()

points = []
for line in lines:
    a, b = ints(line)
    points.append(Vec2(a, b))

minx = min(p.x for p in points)
maxx = max(p.x for p in points)
miny = min(p.y for p in points)
maxy = max(p.y for p in points)

s = {}
for x in range(minx, maxx + 1):
    for y in range(miny, maxy + 1):
        distances = [(i, (Vec2(x, y) - p).manhattan()) for i, p in enumerate(points)]
        i, max_dist = min(distances, key=lambda v: v[1])
        if sum(v == max_dist for _, v in distances) == 1:
            s[Vec2(x, y)] = i

edges_is = set()
for x in range(minx, maxx + 1):
    for y in [miny, maxy]:
        p = Vec2(x, y)
        if p in s:
            edges_is.add(s[p])
for x in [minx, maxx]:
    for y in range(miny, maxy + 1):
        p = Vec2(x, y)
        if p in s:
            edges_is.add(s[p])

counts = Counter(s.values())
for edge in edges_is:
    counts.pop(edge)

puzzle.answer_a = max(counts.values())

visited: set[Vec2] = {Vec2(200, 200)}
q = [Vec2(200, 200)]
while q:
    p = q.pop()

    for n in [p + delta for delta in Vec2.delta_4()]:
        if n in visited:
            continue

        d = sum((n - p).manhattan() for p in points)

        if d < 10000:
            q.append(n)
            visited.add(n)

puzzle.answer_b = len(visited)
