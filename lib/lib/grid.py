from typing import Sequence

import lib


def neighbours4(pos: tuple[int, int]) -> list[tuple[int, int]]:
    return [lib.tup_add(pos, d) for d in [(0, -1), (0, 1), (-1, 0), (1, 0)]]


def neighbours8(pos: tuple[int, int]) -> list[tuple[int, int]]:
    return [
        lib.tup_add(pos, d)
        for d in [
            (0, -1),
            (1, -1),
            (1, 0),
            (1, 1),
            (0, 1),
            (-1, 1),
            (-1, 0),
            (-1, -1),
        ]
    ]


def print_set_grid(
    grid: set[tuple[int, int]] | dict[tuple[int, int], str | int],
    empty: str = ".",
    min_x: int | None = None,
    min_y: int | None = None,
    max_x: int | None = None,
    max_y: int | None = None,
):
    xs, ys = lib.unzip(grid)
    min_x = min(xs) if min_x is None else min_x - 1
    max_x = max(xs) if max_x is None else max_x - 1
    min_y = min(ys) if min_y is None else min_y - 1
    max_y = max(ys) if max_y is None else max_y - 1

    get = (
        (lambda k: "#" if k in grid else empty)
        if isinstance(grid, set)
        else (lambda k: str(grid.get(k, empty)))  # type: ignore
    )

    for y in range(min_y, max_y + 1):
        line = ""
        for x in range(min_x, max_x + 1):
            line += get((x, y))
        print(line)


def print_grid(grid: Sequence[Sequence[str | int]], transpose=False):
    if not transpose:
        for line in grid:
            print("".join(map(str, line)))
    else:
        for y in range(len(grid[0])):
            for x in range(len(grid)):
                print(grid[x][y], end="")
            print()
