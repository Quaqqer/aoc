# 15   00:30:05   1465      0   02:29:12   4089      0
# Part 2 took so long because I forgot to parse negative numbers... Kill me please

import re

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("15"))
id = puzzle.input_data


def manhattan_distance(pos1, pos2):
    x1, y1 = pos1
    x2, y2 = pos2
    return abs(x1 - x2) + abs(y1 - y2)


sensors = {}
for line in id.splitlines():
    sensor_x, sensor_y, beacon_x, beacon_y = map(int, re.findall(r"-?\d+", line))
    sensors[sensor_x, sensor_y] = beacon_x, beacon_y

sensor_beacons = list(sensors.items())
sensor_beacons.sort(key=lambda x: manhattan_distance(*x))


maxxy = 4000000


def edge(sensor, range_):
    points = set()
    sx, sy = sensor

    drange = range_ + 1

    for dx in range(-drange, drange + 1):
        for dy in [drange - abs(dx), -(drange - abs(dx))]:
            x, y = sx + dx, sy + dy
            if 0 <= x < maxxy and 0 <= y < maxxy:
                points.add((sx + dx, sy + dy))

    return points


def prune(points):
    for sensor, beacon in sensor_beacons[::-1]:
        range_ = manhattan_distance(sensor, beacon)

        if not points:
            return

        for point in points.copy():
            dist = manhattan_distance(point, sensor)
            if dist <= range_:
                points.remove(point)


def ans_b():
    for sensor, beacon in sensor_beacons:
        range_ = manhattan_distance(sensor, beacon)
        diamond = edge(sensor, range_)
        prune(diamond)
        if len(diamond) == 1:
            x, y = diamond.pop()
            return x * 4000000 + y


puzzle.answer_b = ans_b()
