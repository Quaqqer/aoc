#!/usr/bin/env python3

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2017, int("03"))
id = puzzle.input_data


# Main code
def square_coords(square_nr: int) -> tuple[int, int]:
    (x, y) = (0, 0)
    (dx, dy) = (1, 0)

    i = 1
    a = 1
    while True:
        for _ in range((a + 1) // 2):
            if i == square_nr:
                return (x, y)

            (x, y) = (x + dx, y + dy)
            i += 1
        a += 1

        (dx, dy) = (dy, -dx)


square_nr = int(id)
puzzle.answer_a = sum(map(abs, list(square_coords(square_nr))))
