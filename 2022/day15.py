# 15   00:30:05   1465      0   02:29:12   4089      0
# Part 2 took so long because I forgot to parse negative numbers... Kill me please

import re

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("15"))
id = puzzle.input_data

point = tuple[int, int]

sbds: list[tuple[point, point, int]] = []
for line in id.splitlines():
    sensor_x, sensor_y, beacon_x, beacon_y = lib.all_ints(line)
    sensor = sensor_x, sensor_y
    beacon = beacon_x, beacon_y
    sbds.append((sensor, beacon, lib.grid.manhattan_distance(sensor, beacon)))
sbds.sort(key=lambda e: e[2])


def ans_a():
    line = 2000000

    visible_line: set[int] = set()
    for sensor, _, range_ in sbds:
        dy = abs(sensor[1] - line)
        for dx in range(range_ - dy + 1):
            visible_line |= {sensor[0] + dx, sensor[0] - dx}

    sensors, beacons, _ = lib.unzip(sbds)
    length = len(visible_line - set(p[0] for p in sensors + beacons if p[1] == line))
    return length


def diamond(sensor: point, range_: int) -> set[point]:
    maxxy = 4000000

    points = set()
    sx, sy = sensor

    drange = range_ + 1

    for dx in range(-drange, drange + 1):
        for dy in [drange - abs(dx), -(drange - abs(dx))]:
            x, y = sx + dx, sy + dy
            if 0 <= x < maxxy and 0 <= y < maxxy:
                points.add((sx + dx, sy + dy))

    return points


def prune(points: set[point]):
    for sensor, _, range_ in sbds[::-1]:
        for point in points.copy():
            dist = lib.grid.manhattan_distance(point, sensor)
            if dist <= range_:
                points.remove(point)


def ans_b():
    for sensor, _, range_ in sbds:
        possibilities = diamond(sensor, range_)
        prune(possibilities)

        if possibilities:
            x, y = possibilities.pop()
            return x * 4000000 + y


puzzle.answer_a = ans_a()
puzzle.answer_b = ans_b()
