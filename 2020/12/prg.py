#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=12);
input = puzzle.input_data.split("\n")

x, y = 0, 0
facing = (1, 0)
for i in input:
    v = int(i[1:])
    if i[0] == "F":
        d = facing[0] * v, facing[1] * v
        x, y = x + d[0], y + d[1]
    elif i[0] == "L":
        for j in range(v//90):
            facing = (facing[1], -facing[0])
    elif i[0] == "R":
        for j in range(v//90):
            facing = (-facing[1], facing[0])
    elif i[0] == "N":
        x, y = x + 0 * v, y + -1 * v
    elif i[0] == "S":
        x, y = x + 0 * v, y + 1 * v
    elif i[0] == "E":
        x, y = x + 1 * v, y + 0 * v
    elif i[0] == "W":
        x, y = x + -1 * v, y + 0 * v

silver = abs(x) + abs(y)
print(silver)

# Gold
x, y = 0, 0
waypoint = 10, -1
for i in input:
    v = int(i[1:])
    if i[0] == "F":
        d = waypoint[0] * v, waypoint[1] * v
        x, y = x + d[0], y + d[1]
    elif i[0] == "L":
        for j in range(v//90):
            waypoint = (waypoint[1], -waypoint[0])
    elif i[0] == "R":
        for j in range(v//90):
            waypoint = (-waypoint[1], waypoint[0])
    elif i[0] == "N":
        waypoint = waypoint[0] + 0 * v, waypoint[1] + -1 * v
    elif i[0] == "S":
        waypoint = waypoint[0] + 0 * v, waypoint[1] + 1 * v
    elif i[0] == "E":
        waypoint = waypoint[0] + 1 * v, waypoint[1] + 0 * v
    elif i[0] == "W":
        waypoint = waypoint[0] + -1 * v, waypoint[1] + 0 * v
    print(f"Instruction: {i}")
    print(f"x: {x}, y: {y}, wp: {waypoint}")

gold = abs(x) + abs(y)
print(gold)
