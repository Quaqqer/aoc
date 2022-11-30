#!/usr/bin/env python3
from aocd.models import Puzzle

puzzle = Puzzle(2017, int("02"))
id = puzzle.input_data


def row_checksum(cols: list[int]) -> int:
    return max(cols) - min(cols)


# Main code
puzzle.answer_a = sum(
    row_checksum([int(c) for c in cols_str.split() if c])
    for cols_str in id.splitlines()
)


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
