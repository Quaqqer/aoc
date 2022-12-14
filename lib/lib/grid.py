from typing import Iterable, Sequence

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


def print_set_grid(grid: set[tuple[int, int]] | dict[tuple[int, int], str | int]):
    xs, ys = lib.unzip(grid)
    min_x = min(xs)
    max_x = max(xs)
    min_y = min(ys)
    max_y = max(ys)

    get = (
        (lambda k: "#" if k in grid else ".")
        if isinstance(grid, set)
        else (lambda k: str(grid.get((k, " "))))  # type: ignore
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
