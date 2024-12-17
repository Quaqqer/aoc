# 17   00:16:39   751      0   02:09:19  1607      0
# Jesus, part 2 was way too hard for a tuesday

from typing import cast

import z3
from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2024, int("17"))
data = puzzle.input_data


def run(a: int, b: int, c: int, pgm: list[int]) -> int:
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

    out = 0
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
                out = out * 10 + get_combo(v) % 8
            case 6:
                b = a >> get_combo(v)
            case 7:
                c = a >> get_combo(v)
            case _:
                raise Exception()
        i += 2
    return out


def find_a(
    b: int | z3.BitVecRef, c: int | z3.BitVecRef, pgm: list[int], answer: int
) -> int:
    # I reverse engineered the code so I know we need 15 jumps
    jumps = 15

    solver = z3.Solver()
    a_init = z3.BitVec("a_init", 100)
    solver.add(a_init >= 8**15, a_init < 8**16)

    i = 0
    a = a_init
    b = b
    c = c

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

    out_digit = 0

    jumps = 15
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
                if jumps > 0:
                    jumps -= 1
                    i = v
                    continue
            case 4:
                b = b ^ c
            case 5:
                solver.append(int(str(answer)[15 - jumps]) == get_combo(v) % 8)
                out_digit += 1
            case 6:
                b = a >> get_combo(v)
            case 7:
                c = a >> get_combo(v)
            case _:
                raise Exception()
        i += 2

    solver.check()
    m = solver.model()
    return cast(int, m[a_init])


a, b, c, *pgm = ints(data)
puzzle.answer_a = ",".join(str(run(a, b, c, pgm)))
puzzle.answer_b = find_a(b, c, pgm, int("".join(map(str, pgm))))
