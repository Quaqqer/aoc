# 13   00:07:27   343      0   00:21:36   558      0

import numpy as np
import numpy.linalg as linalg
from aocd.models import Puzzle
from util import floats

puzzle = Puzzle(2024, int("13"))

data = puzzle.input_data

lines = data.splitlines()
EPSILON = 1e-3


def solve(part_b: bool):
    s = 0
    for chunk in data.split("\n\n"):
        ax, ay, bx, by, px, py = floats(chunk)
        prize = np.array([px, py])
        if part_b:
            prize += 10000000000000
        basis = np.array([[ax, bx], [ay, by]])
        conversion = linalg.inv(basis)
        a, b = conversion @ prize
        if abs(a - round(a)) < EPSILON and abs(b - round(b)) < EPSILON:
            s += a * 3 + b
    return s


puzzle.answer_a = solve(False)
puzzle.answer_b = solve(True)
