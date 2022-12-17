from typing import Sequence as _Sequence

import lib

point = tuple[int, int]


def neighbours4(pos: point) -> list[point]:
    return [lib.tup_add(pos, d) for d in [(0, -1), (0, 1), (-1, 0), (1, 0)]]


def neighbours8(pos: point) -> list[point]:
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
    grid: set[point] | dict[point, str | int],
    empty: str = ".",
    min_x: int | None = None,
    min_y: int | None = None,
    max_x: int | None = None,
    max_y: int | None = None,
):
    xs, ys = lib.unzip(grid)
    minx: int = min(xs) if min_x is None else min_x - 1
    maxx: int = max(xs) if max_x is None else max_x - 1
    miny: int = min(ys) if min_y is None else min_y - 1
    maxy: int = max(ys) if max_y is None else max_y - 1

    get = (
        (lambda k: "#" if k in grid else empty)
        if isinstance(grid, set)
        else (lambda k: str(grid.get(k, empty)))  # type: ignore
    )

    for y in range(miny, maxy + 1):
        line = ""
        for x in range(minx, maxx + 1):
            line += get((x, y))
        print(line)


def print_grid(grid: _Sequence[_Sequence[str | int]], transpose=False):
    if not transpose:
        for line in grid:
            print("".join(map(str, line)))
    else:
        for y in range(len(grid[0])):
            for x in range(len(grid)):
                print(grid[x][y], end="")
            print()


def manhattan(point1: point, point2: point) -> int:
    x1, y1 = point1
    x2, y2 = point2
    return abs(x1 - x2) + abs(y1 - y2)
