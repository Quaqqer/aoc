# Part 2 of this day is kind of stupid :(

import math
from copy import copy

from aocd.models import Puzzle

puzzle = Puzzle(2019, int("16"))
data = puzzle.input_data

ns = list(map(int, data))
offset = int(data[:7])

muls = [0, 1, 0, -1]


def create_muls(n: int) -> list[int]:
    return [muls[(i // (n + 1)) % 4] for i in range(len(ns) + 1)][1:]


def digit(n: int) -> int:
    if n < 0:
        return n % -10
    else:
        return n % 10


a_ns = copy(ns)
for i in range(100):
    a_ns = [
        digit(sum(digit(a * b) for a, b in zip(a_ns, create_muls(i))))
        for i in range(len(ns))
    ]

puzzle.answer_a = "".join(map(str, a_ns))[:7]


assert offset > math.ceil(len(ns) / 2)

b_ns = copy(ns) * 10_000
for x in range(100):
    print(x)
    running = 0
    for i, v in list(enumerate(b_ns))[::-1]:
        running = (running + v) % 10
        b_ns[i] = running

puzzle.answer_b = "".join(str(b_ns[offset + i]) for i in range(8))
