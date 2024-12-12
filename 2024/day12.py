# 12   00:09:41   683      0   00:28:30   544      0
# Pretty fun day today. I realize I just think days are fun if I do reasonably
# well in them.


from aocd.models import Puzzle
from util import Grid, Vec2

puzzle = Puzzle(2024, int("12"))

data = puzzle.input_data
g = Grid.from_lines(data)

visited = set()


def explore(p: Vec2) -> tuple[set[tuple[Vec2, Vec2]], int]:
    """Explore a point, recursively find the whole area and borders

    Borders are stored in a set of border position, and a delta to check the
    directionality of the border. Hopefully that makes sense.
    """
    visited.add(p)

    border: set[tuple[Vec2, Vec2]] = set()
    squares = 1

    for n in map(Vec2, g.neighbours4(p.tup, inside=False)):
        if n in g and g[n] == g[p]:
            if n not in visited:
                xs, xamt = explore(n)
                border |= xs
                squares += xamt
        else:
            border |= {(n, n - p)}

    return border, squares


def count_edges(border: set[tuple[Vec2, Vec2]]):
    edges = 0
    for p, delta in border:
        if not ((p + Vec2(1, 0), delta) in border or (p + Vec2(0, 1), delta) in border):
            edges += 1
    return edges


a = 0
b = 0
for p in map(Vec2, g):
    if p not in visited:
        border, squares = explore(p)
        a += len(border) * squares
        b += count_edges(border) * squares

puzzle.answer_a = a
puzzle.answer_b = b
