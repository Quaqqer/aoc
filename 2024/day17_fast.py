# Solve it again without using z3, it was tricky. The last digit of the output
# depends on the first digit, the second last depends on the first and second,
# etc. The n:th digit depends last n:th digit and a few before. To find it we
# check that the prefix is okay. Since we are using xor we don't need to be too
# smart, if it had been a really bad input a DP solution might have been in
# order.

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2024, int("17"))
data = puzzle.input_data


def run(a: int, b: int, c: int, pgm: list[int]) -> list[int]:
    i = 0

    def get_combo(v):
        if v < 4:
            return v
        elif v == 4:
            return a
        elif v == 5:
            return b
        elif v == 6:
            return c
        else:
            raise Exception()

    out = []
    while i < len(pgm):
        op, v = pgm[i], pgm[i + 1]
        match op:
            case 0:
                a = a >> get_combo(v)
            case 1:
                b = b ^ v
            case 2:
                b = get_combo(v) % 8
            case 3:
                if a != 0:
                    i = v
                    continue
            case 4:
                b = b ^ c
            case 5:
                out += [get_combo(v) % 8]
            case 6:
                b = a >> get_combo(v)
            case 7:
                c = a >> get_combo(v)
            case _:
                raise Exception()
        i += 2
    return out


def solve_a():
    def inner(a: int, i: int):
        if i == len(pgm):
            return a

        for d in range(8):
            if run(a << 3 | d, 0, 0, pgm) == pgm[-i - 1 :]:
                x = inner(a << 3 | d, i + 1)
                if x is not None:
                    return x

    ans = inner(0, 0)
    assert ans is not None
    return ans


a, b, c, *pgm = ints(data)
puzzle.answer_a = ",".join(map(str, run(a, b, c, pgm)))
puzzle.answer_b = solve_a()
