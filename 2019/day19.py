from typing import Iterator

import util
from aocd.models import Puzzle
from intpc import IntPC
from util import Vec2

puzzle = Puzzle(2019, int("19"))
data = puzzle.input_data
program = util.ints(data.split(","))


def look(x: int, y: int) -> bool:
    [out] = IntPC(program).io([x, y])
    return out == 1


def find_b() -> int:
    def upper_line(start_x: int) -> Iterator[Vec2[int]]:
        x, y = start_x - 1, 0

        while True:
            x += 1

            while not look(x, y):
                y += 1

            yield Vec2(x, y)

    def lower_line(start_y: int) -> Iterator[Vec2[int]]:
        x, y = 0, start_y - 1

        while True:
            y += 1

            while not look(x, y):
                x += 1

            yield Vec2(x, y)

    upper = upper_line(10)
    lower = lower_line(10)

    up = next(upper)
    low = next(lower)

    while True:
        if low.y - up.y < 99:
            low = next(lower)
        elif up.x - low.x < 99:
            up = next(upper)
        else:
            return low.x * 10_000 + up.y


puzzle.answer_a = sum(look(x, y) for x in range(50) for y in range(50))
puzzle.answer_b = find_b()
