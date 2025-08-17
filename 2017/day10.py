import util
from aocd.models import Puzzle

puzzle = Puzzle(2017, int("10"))


def flip(l: list[int], pos: int, n: int):
    vals = [l[p % len(l)] for p in range(pos, pos + n)]
    for i, v in enumerate(vals):
        l[(pos + n - i - 1) % len(l)] = v


def solve_a():
    flip_list = list(map(int, puzzle.input_data.split(",")))

    l = list(range(256))
    skip = 0

    pos = 0
    for v in flip_list:
        flip(l, pos, v)
        pos += v + skip
        skip += 1

    return l[0] * l[1]


def solve_b():
    flip_list = list(map(ord, puzzle.input_data)) + [17, 31, 73, 47, 23]

    l = list(range(256))
    skip = 0

    pos = 0
    for v in flip_list * 64:
        flip(l, pos, v)
        pos += v + skip
        skip += 1

    dense = []
    for chunk in util.chunk(l, 16):
        n = 0
        for v in chunk:
            n ^= v
        dense.append(n)
    return "".join(hex(v)[2:].rjust(2, "0") for v in dense)


solve_a()


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
