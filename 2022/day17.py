#!/usr/bin/env python3
import time

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("17"))
id = puzzle.input_data
id = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"


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


def draw(dropped, rock, rockx, rocky):
    to_draw = {p: "#" for p in dropped}
    to_draw.update({p: "@" for p in insert_dropped(rock, rockx, rocky)[0]})
    lib.grid.print_set_grid(to_draw)  # type: ignore
    print()
    time.sleep(0.1)


i = 0
firsts = {}
done = False
while i < 2022:
    rock = rocks[i % 5]

    rwidth = len(rock[0])
    rheight = len(rock)

    rockx = 2
    rocky = height - 3

    while True:
        if shift() == "<":
            # print("<")
            if 0 < rockx:
                if not touches_dropped(dropped, rock, rockx - 1, rocky):
                    rockx -= 1
                    # draw(dropped, rock, rockx, rocky)
        else:
            # print(">")
            if rockx < 7 - rwidth:
                if not touches_dropped(dropped, rock, rockx + 1, rocky):
                    rockx += 1
                    # draw(dropped, rock, rockx, rocky)

        if touches_dropped(dropped, rock, rockx, rocky + 1):
            break
        rocky += 1
        # print("v")
        # draw(dropped, rock, rockx, rocky)

    dropped_rock, mheight = insert_dropped(rock, rockx, rocky)
    # print(i)
    # draw(dropped, rock, rockx, rocky)

    dropped |= dropped_rock
    height = min(height, mheight)

    i += 1

puzzle.answer_a = -height
