#!/usr/bin/env python3
from aocd.models import Puzzle

puzzle = Puzzle(2017, int("02"))
id = puzzle.input_data


def row_checksum(cols: list[int]) -> int:
    return max(cols) - min(cols)


# Main code
puzzle.answer_a = sum(
    max(row) - min(row)
    for row in [[int(cell) for cell in line.split("\t")] for line in id.splitlines()]
)

puzzle.answer_b = sum(
    v1 // v2
    for row in [[int(c) for c in row.split("\t")] for row in id.splitlines()]
    for v1 in row
    for v2 in row
    if v1 % v2 == 0 and v1 != v2
)
