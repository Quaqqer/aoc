from aocd.models import Puzzle
from util import Grid

puzzle = Puzzle(2019, int("24"))
data = puzzle.input_data

g = Grid.from_lines(data)


def step(g: Grid[str]) -> Grid[str]:
    neigh_counts = g.map_with_pos(
        lambda pos, _: sum(g[n] == "#" for n in g.neighbours4(pos))
    )

    def new_cell(pos: tuple[int, int], v: str) -> str:
        if v == "#":
            return "#" if neigh_counts[pos] == 1 else "."
        else:
            return "#" if neigh_counts[pos] in (1, 2) else "."

    return g.map_with_pos(new_cell)


seen = {g}

while True:
    g = step(g)

    if g in seen:
        break

    seen.add(g)

puzzle.answer_a = sum(2**i for i in range(25) if g[i % 5, i // 5] == "#")
