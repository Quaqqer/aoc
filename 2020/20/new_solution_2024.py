import math
from collections import defaultdict
from typing import Literal, cast

from aocd.models import Puzzle
from util import Grid, ints

puzzle = Puzzle(2020, int("20"))
data = puzzle.input_data


def r[T](g: Grid[T]) -> Grid[T]:
    return g.rotate_right()


# Parse grids
grids: dict[int, Grid[str]] = {}
for group in data.split("\n\n"):
    lines = group.splitlines()
    [id] = ints(lines[0])

    grid = Grid.from_lines("\n".join(lines[1:]))

    grids[id] = grid

# Find all variants of a grid
variantss: dict[int, list[Grid[str]]] = defaultdict(list)
for id, grid in grids.items():
    for flip in [grid, grid.mirror("x")]:
        for rotation in [flip, r(flip), r(r(flip)), r(r(r(flip)))]:
            variantss[id].append(rotation)

type Dir = Literal["n"] | Literal["s"] | Literal["e"] | Literal["w"]

# Which rows connect to which (id, variant)
edges: dict[tuple[tuple[str, ...], Dir], list[tuple[int, int]]] = defaultdict(list)

for variant_id, variants in variantss.items():
    for variant_i, grid in enumerate(variants):
        edges[tuple(grid[:, 0]), "n"].append((variant_id, variant_i))
        edges[tuple(grid[:, grid.rows - 1]), "s"].append((variant_id, variant_i))
        edges[tuple(grid[0, :]), "w"].append((variant_id, variant_i))
        edges[tuple(grid[grid.cols - 1, :]), "e"].append((variant_id, variant_i))

TILE_W = 10
TILES_W = int(math.sqrt(len(grids)))


def find_grid() -> Grid[tuple[int, int]]:
    # Used grids
    used: set[int] = set()
    assigned: Grid[tuple[int, int] | None] = Grid.new_fill(
        lambda: None, TILES_W, TILES_W
    )

    def inner(i: int) -> bool:
        if i == TILES_W * TILES_W:
            return True

        candidates = set(
            {(i, j) for i, vs in variantss.items() for j, _ in enumerate(vs)}
        )

        y = i // TILES_W
        x = i % TILES_W

        if 0 < y:
            above = assigned[x, y - 1]
            assert above is not None
            above_id, above_variant = above
            above_grid = variantss[above_id][above_variant]
            above_edge = tuple(above_grid[:, above_grid.rows - 1])
            candidates &= set(edges[above_edge, "n"])

        if 0 < x:
            left = assigned[x - 1, y]
            assert left is not None
            left_id, left_variant = left
            left_grid = variantss[left_id][left_variant]
            left_edge = tuple(left_grid[left_grid.cols - 1, :])
            candidates &= set(edges[left_edge, "w"])

        for candidate_id, candidate_variant in candidates:
            if candidate_id in used:
                continue

            used.add(candidate_id)
            assigned[x, y] = candidate_id, candidate_variant

            if inner(i + 1):
                return True

            used.remove(candidate_id)

        return False

    res = inner(0)
    assert res

    return cast(Grid[tuple[int, int]], assigned)


solution = find_grid()
puzzle.answer_a = (
    solution[0, 0][0] * solution[0, -1][0] * solution[-1, 0][0] * solution[-1, -1][0]
)


seamonster = Grid.from_lines("""                  # 
#    ##    ##    ###
 #  #  #  #  #  #   """)
seamonster_variants: list[Grid[str]] = []
for flip in [seamonster, seamonster.mirror("x")]:
    for rotation in [flip, r(flip), r(r(flip)), r(r(r(flip)))]:
        seamonster_variants.append(rotation)


def find_and_mark_seamonsters(g: Grid[str]):
    for seamonster in seamonster_variants:
        for x in range(g.cols - seamonster.cols):
            for y in range(g.rows - seamonster.rows):
                found = all(
                    g[x + dx, y + dy] == "#" for dx, dy in seamonster.find_value("#")
                )

                if found:
                    for dx, dy in seamonster.find_value("#"):
                        g[x + dx, y + dy] = "O"

    return g


META_W = (TILE_W - 2) * TILES_W
meta_grid: Grid[str] = Grid.new_fill(lambda: " ", META_W, META_W)

for sx, sy in solution.coords():
    id, variant = solution[sx, sy]
    g = variantss[id][variant]
    for gx in range(1, g.cols - 1):
        for gy in range(1, g.rows - 1):
            meta_grid[sx * (TILE_W - 2) + gx - 1, sy * (TILE_W - 2) + gy - 1] = g[
                gx, gy
            ]

find_and_mark_seamonsters(meta_grid)
puzzle.answer_b = len(list(meta_grid.find_value("#")))
