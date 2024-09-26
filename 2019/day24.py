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


def counts2(grids: list[Grid[str]]) -> list[Grid[int]]:
    counts = [Grid.new_fill(int, 5, 5) for _ in grids]

    for i in range(len(grids)):
        g = grids[i]
        count = counts[i]

        for pos in g.coords():
            # Add count as usual
            count[pos] += sum(g[p] == "#" for p in g.neighbours4(pos))

        # Add outer
        outer = i - 1
        if 0 <= outer:
            for y in range(5):
                count[0, y] += int(grids[outer][1, 2] == "#")
                count[4, y] += int(grids[outer][3, 2] == "#")
            for x in range(5):
                count[x, 0] += int(grids[outer][2, 1] == "#")
                count[x, 4] += int(grids[outer][2, 3] == "#")

        # Add inner
        inner = i + 1
        if inner < len(grids):
            for y in range(5):
                count[1, 2] += int(grids[inner][0, y] == "#")
                count[3, 2] += int(grids[inner][4, y] == "#")
            for x in range(5):
                count[2, 1] += int(grids[inner][x, 0] == "#")
                count[2, 3] += int(grids[inner][x, 4] == "#")

        count[2, 2] = 0

    return counts


def step2(grids: list[Grid[str]]) -> list[Grid[str]]:
    def new_cell(g: Grid[str], pos: tuple[int, int], count: int) -> str:
        if pos == (2, 2):
            return "."

        if g[pos] == "#":
            return "#" if count == 1 else "."
        else:
            return "#" if count in (1, 2) else "."

    empty_grid = Grid.new_fill(lambda: ".", 5, 5)

    new_grids = [empty_grid.copy()] + grids + [empty_grid.copy()]
    counts = counts2(new_grids)

    n_grids = len(new_grids)

    new_grids = [
        counts[i].map_with_pos(lambda pos, v: new_cell(new_grids[i], pos, v))
        for i in range(n_grids)
    ]

    while all(v == "." for v in new_grids[0].values()):
        new_grids = new_grids[1:]

    while all(v == "." for v in new_grids[-1].values()):
        new_grids = new_grids[:-1]

    return new_grids


seen = {g}

while True:
    g = step(g)

    if g in seen:
        break

    seen.add(g)

puzzle.answer_a = sum(2**i for i in range(25) if g[i % 5, i // 5] == "#")


gs = [Grid.from_lines(data)]
gs[0][2, 2] = "."

for _ in range(200):
    gs = step2(gs)

puzzle.answer_b = sum(c == "#" for g in gs for c in g.values())
