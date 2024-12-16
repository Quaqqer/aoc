# 16   00:28:50  1998      0   00:33:36   825      0
# Overslept today. Woke up 6.16 (opens 6.00 in Sweden), looked at my phone and
# panicked!


from aocd.models import Puzzle
from util import Grid, Heap, Vec2

puzzle = Puzzle(2024, int("16"))

data = puzzle.input_data

g = Grid.from_lines(data)

[start] = map(Vec2, g.find_value("S"))
[end] = map(Vec2, g.find_value("E"))
g[start] = "."
g[end] = "."


def search() -> tuple[int, int]:
    q = Heap([(0, start, Vec2(1, 0), [start])])
    visited = {(start, Vec2(1, 0))}

    best = None
    points = set()
    while q:
        score, p, d, path = q.pop()
        visited.add((p, d))

        if best is not None and score > best:
            return best, len(points)
        if p == end:
            best = score
            points |= set(path)

        if g[p + d] == ".":
            if (p + d, d) not in visited:
                q.push((score + 1, p + d, d, path + [p + d]))

        for ndir in [d.rot_l(), d.rot_r()]:
            if (p, ndir) not in visited:
                q.push((score + 1000, p, ndir, path))
    raise Exception()


puzzle.answers = search()
