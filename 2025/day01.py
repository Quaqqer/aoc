# pyright: basic

from aocd.models import Puzzle

puzzle = Puzzle(2025, int("01"))

data = puzzle.input_data

lines = data.splitlines()


def solve_a():
    dial = 50
    s = 0

    for line in lines:
        d = line[0]
        n = int(line[1:])

        dial = ((dial - n) if d == "L" else (dial + n)) % 100
        if dial == 0:
            s += 1

    return s


def solve_b():
    dial = 50
    s = 0

    for line in lines:
        d = line[0]
        n = int(line[1:])

        for i in range(n):
            dial = (dial - 1) if d == "L" else (dial + 1)
            dial %= 100
            if dial == 0:
                s += 1

    return s


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
