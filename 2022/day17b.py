#!/usr/bin/env python3
import time

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("17"))
id = puzzle.input_data
# id = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"


s = 0


def shift():
    global s
    v = id[s % len(id)]
    s += 1
    return v


def touches_dropped(dropped, rock, rockx, rocky) -> bool:
    h = len(rock)
    for y, row in enumerate(rock):
        for x, c in enumerate(row):
            if c != " ":
                px, py = rockx + x, rocky - h + y
                if (px, py) in dropped:
                    return True
    return False


def insert_dropped(rock, rockx, rocky):
    h = len(rock)
    maxh = 0
    new = set()

    for y, row in enumerate(rock):
        for x, c in enumerate(row):
            if c != " ":
                px, py = rockx + x, rocky - h + y
                maxh = min(maxh, py)
                new.add((px, py))

    return new, maxh


rocks = [
    ["####"],
    [" # ", "###", " # "],
    ["  #", "  #", "###"],
    ["#", "#", "#", "#"],
    ["##", "##"],
]

dropped = set((x, 0) for x in range(7))
height = 0


i = 0
firsts = {}
done = False

add_i = 0
add_height = 0

firsts = {}

while i < 1000000000000:
    rock = rocks[i % 5]

    rwidth = len(rock[0])
    rheight = len(rock)

    rockx = 2
    rocky = height - 3

    while True:
        if shift() == "<":
            if 0 < rockx:
                if not touches_dropped(dropped, rock, rockx - 1, rocky):
                    rockx -= 1
        else:
            if rockx < 7 - rwidth:
                if not touches_dropped(dropped, rock, rockx + 1, rocky):
                    rockx += 1

        if touches_dropped(dropped, rock, rockx, rocky + 1):
            break
        rocky += 1

    dropped_rock, mheight = insert_dropped(rock, rockx, rocky)
    dropped |= dropped_rock
    height = min(height, mheight)

    line_id = tuple(
        (x, y) for x in range(7) for y in range(200) if (x, y + height) in dropped
    )
    looper = s % len(id), line_id

    if looper not in firsts:
        firsts[looper] = i, height
    else:
        fi, fheight = firsts[looper]
        height += (height - fheight) * ((1000000000000 - i) // (i - fi))
        i += (i - fi) * ((1000000000000 - i) // (i - fi))
        d = {(x, y + height) for (x, y) in line_id}
        dropped |= d

    i += 1


puzzle.answer_b = -height
