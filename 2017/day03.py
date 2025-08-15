#!/usr/bin/env python3
from collections import defaultdict

from aocd.models import Puzzle

puzzle = Puzzle(2017, int("03"))
id = int(puzzle.input_data)


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


def b_first_larger_square(id: int) -> int:
    (x, y) = (0, 0)
    (dx, dy) = (1, 0)

    squares: dict[tuple[int, int], int] = defaultdict(int)
    squares[0, 0] = 1

    i = 1
    a = 1
    while True:
        for _ in range((a + 1) // 2):
            squares[x, y] = sum(
                squares[x + dx, y + dy] for dx in (-1, 0, 1) for dy in (-1, 0, 1)
            )

            if squares[x, y] > id:
                return squares[x, y]
            (x, y) = (x + dx, y + dy)
            i += 1
        a += 1

        (dx, dy) = (dy, -dx)


puzzle.answer_a = sum(map(abs, list(square_coords(id))))
puzzle.answer_b = b_first_larger_square(id)
