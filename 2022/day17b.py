# 17   00:47:50    880      0   02:47:11   1718      0
# not the best times, I was pretty braindead
# modulus is kinda sus tbh

from typing import Sequence

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("17"))

shifts = puzzle.input_data

shift_i = 0

rocks = [
    ["####"],
    [" # ", "###", " # "],
    ["  #", "  #", "###"],
    ["#", "#", "#", "#"],
    ["##", "##"],
]


def touches_dropped(
    dropped: set[lib.grid.point], rock: Sequence[Sequence[str]], rockx: int, rocky: int
) -> bool:
    h = len(rock)
    for y, row in enumerate(rock):
        for x, c in enumerate(row):
            if c != " ":
                px, py = rockx + x, rocky - h + y
                if (px, py) in dropped:
                    return True
    return False


def insert_dropped(
    rock: Sequence[Sequence[str]], rockx: int, rocky: int
) -> tuple[set[lib.grid.point], int]:
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


def shift() -> str:
    global shift_i
    v = shifts[shift_i]
    shift_i = (shift_i + 1) % len(shifts)
    return v


dropped = set((x, 0) for x in range(7))  # Initially the floor
height = 0  # Negative height, because coordinate systems
firsts = {}  #

drop_i = 0
while drop_i < 1000000000000:
    # Get the current rock to drop
    rock = rocks[drop_i % 5]

    # Get width and height of rock
    rwidth = len(rock[0])
    rheight = len(rock)

    # Initial rock position
    rockx = 2
    rocky = height - 3

    # Move rock, first shift it and then try to move down
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

    # Insert the rock into the dropped set and update height
    dropped_rock, mheight = insert_dropped(rock, rockx, rocky)
    dropped |= dropped_rock
    height = min(height, mheight)

    # Get an identifier for the line, by taking the top 200 rows.
    # Pray to god that 200 is enough, because it could fail.
    # Maybe not the best way to do this, but it works and is not too complicated.
    line_id = tuple(
        (x, y) for x in range(7) for y in range(200) if (x, y + height) in dropped
    )
    # Get the loop identifier, the shift position and the line id
    looper = shift_i, line_id

    if looper not in firsts:
        # Insert the loop identifier if not already in there
        firsts[looper] = drop_i, height
    elif drop_i > 2022:
        # If the loop has been identifier we skip forward
        fi, fheight = firsts[looper]
        height += (height - fheight) * ((1000000000000 - drop_i) // (drop_i - fi))
        drop_i += (drop_i - fi) * ((1000000000000 - drop_i) // (drop_i - fi))
        # Insert the copied rows to the new height
        d = {(x, y + height) for (x, y) in line_id}
        dropped |= d

    drop_i += 1

    if drop_i == 2022:
        puzzle.answer_a = -height


puzzle.answer_b = -height
