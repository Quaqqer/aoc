from aocd.models import Puzzle
from util import Grid

puzzle = Puzzle(2015, int("18"))

data = puzzle.input_data
g = Grid.from_lines(data).map(lambda s: s == "#")


def step(g: Grid[bool], b: bool) -> Grid[bool]:
    def on_neighs(pos: tuple[int, int]) -> int:
        return sum(g[n] for n in g.neighbours8(pos))

    g = g.map_with_pos(lambda p, v: on_neighs(p) in [2, 3] if v else on_neighs(p) == 3)
    if b:
        g[0, 0] = True
        g[0, -1] = True
        g[-1, 0] = True
        g[-1, -1] = True

    return g


def solve(g: Grid[bool], b: bool):
    for _ in range(100):
        g = step(g, b)
    return sum(g.values())


puzzle.answer_a = solve(g, False)
puzzle.answer_b = solve(g, True)
