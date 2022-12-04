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
    l_set = set(range(lmin, lmax + 1))
    r_set = set(range(rmin, rmax + 1))

    intersection = l_set & r_set

    if len(intersection) == len(l_set) or len(intersection) == len(r_set):
        sum_a += 1

    if intersection:
        sum_b += 1

puzzle.answer_a = sum_a
puzzle.answer_b = sum_b
