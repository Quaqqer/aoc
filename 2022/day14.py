# 14   00:20:17    646      0   00:23:17    543      0
# pretty good today

import lib
from aocd.models import Puzzle

puzzle = Puzzle(2022, int("14"))
id = puzzle.input_data


def fill_rocks(input_data: str) -> tuple[set[tuple[int, int]], int]:
    rocks_and_sand = set()

    maxy = 0
    for line in input_data.splitlines():
        coords = [
            (int(x), int(y)) for x, y in [c.split(",") for c in line.split(" -> ")]
        ]

        x, y = coords[0]
        rocks_and_sand.add((x, y))
        for newx, newy in coords[1:]:
            while x != newx or y != newy:
                x += lib.sign(newx - x)
                y += lib.sign(newy - y)

                rocks_and_sand.add((x, y))
                maxy = max(maxy, y)

    return rocks_and_sand, maxy


def drop_sand(rocks_and_sand: set[tuple[int, int]], maxy: int, part_b: bool) -> bool:
    x, y = 500, 0

    if (x, y) in rocks_and_sand:
        return False

    while True:
        if part_b and y == maxy + 1:
            break
        elif not part_b and y > maxy:
            return False

        if (x, y + 1) not in rocks_and_sand:
            x, y = x, y + 1
        elif (x - 1, y + 1) not in rocks_and_sand:
            x, y = x - 1, y + 1
        elif (x + 1, y + 1) not in rocks_and_sand:
            x, y = x + 1, y + 1
        else:
            break

    rocks_and_sand.add((x, y))
    return True


rocks_and_sand, maxy = fill_rocks(id)
dropped = 0
while drop_sand(rocks_and_sand, maxy, False):
    dropped += 1
puzzle.answer_a = dropped

while drop_sand(rocks_and_sand, maxy, True):
    dropped += 1
puzzle.answer_b = dropped
