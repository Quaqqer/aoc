#!/usr/bin/env python3
from aocd.models import Puzzle

puzzle = Puzzle(2017, int("02"))
id = puzzle.input_data

# Main code
sum_a = 0
for line in id.splitlines():
    row = [int(c) for c in line.split() if c]
    mi = min(row)
    ma = max(row)
    sum_a += ma - mi
puzzle.answer_a = sum_a


def find_divisor(cs):
    for x in cs:
        for y in cs:
            if x != y:
                res = x / y
                if res - int(res) == 0:
                    return int(x / y)


rows = []
for line in id.splitlines():
    row = [int(c) for c in line.split() if c]
    rows.append(find_divisor(row))
puzzle.answer_b = sum(rows)
