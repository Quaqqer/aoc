# 15   00:30:05   1465      0   02:29:12   4089      0
# Part 2 took so long because I forgot to parse negative numbers... Kill me please

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("15"))
id = puzzle.input_data

Point = tuple[int, int]

sbds: list[tuple[Point, Point, int]] = []
for line in id.splitlines():
    sx, sy, bx, by = lib.all_ints(line)
    sensor = sx, sy
    beacon = bx, by
    range_ = lib.grid.manhattan(sensor, beacon)
    sbds.append((sensor, beacon, range_))


def ans_a():
    line = 2000000

    visible_line: set[int] = set()
    for sensor, _, range_ in sbds:
        dy = abs(sensor[1] - line)
        for dx in range(range_ - dy + 1):
            visible_line |= {sensor[0] + dx, sensor[0] - dx}

    sensors, beacons, _ = lib.unzip(sbds)
    exclude = set(p[0] for p in sensors + beacons if p[1] == line)
    return len(visible_line - exclude)


def ans_b() -> int | None:
    for sensor, _, range_ in sbds:
        maxxy = 4000000

        sx, sy = sensor
        outer = range_ + 1

        for dx in range(-outer, outer + 1):
            for dy in [outer - abs(dx), -(outer - abs(dx))]:
                x, y = sx + dx, sy + dy
                if (
                    0 <= x < maxxy
                    and 0 <= y < maxxy
                    and all(
                        out_of_range((x, y), sensor, range_)
                        for sensor, _, range_ in sbds
                    )
                ):
                    return x * maxxy + y


def out_of_range(point: Point, sensor: Point, range_: int):
    return range_ < lib.grid.manhattan(point, sensor)


puzzle.answer_a = ans_a()
puzzle.answer_b = ans_b()
