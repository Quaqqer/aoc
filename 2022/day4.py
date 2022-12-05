#!/usr/bin/env python3
from aocd.models import Puzzle

puzzle = Puzzle(2022, int("04"))
id = puzzle.input_data

# Main code
sum_a = 0
sum_b = 0

for line in id.splitlines():
    (lmin, lmax), (rmin, rmax) = [
        [int(v) for v in side.split("-")] for side in line.split(",")
    ]

    if (lmin <= rmin and rmax <= lmax) or (rmin <= lmin and lmax <= rmax):
        sum_a += 1

    if not (rmax < lmin or lmax < rmin):
        sum_b += 1

puzzle.answer_a = sum_a
puzzle.answer_b = sum_b
