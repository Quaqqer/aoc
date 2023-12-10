#   9   00:32:58  6238      0   00:36:30  5501      0
# overslept again 😬, but today was a fun puzzle, also my ints helper function
# did not parse negative numbers 😅

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("09"))
data = puzzle.input_data
lines = data.splitlines()


def solve_a(line: str, p2: bool) -> int:
    rows = []

    rows.append(ints(line))

    while not all(v == 0 for v in rows[-1]):
        new_row = []
        for a, b in zip(rows[-1], rows[-1][1:]):
            new_row.append(b - a)
        rows.append(new_row)

    if not p2:
        return sum(row[-1] for row in rows)

    rows[-1] = [0] + rows[-1]
    for i in range(len(rows) - 1)[::-1]:
        rows[i] = [-rows[i + 1][0] + rows[i][0]] + rows[i]

    return rows[0][0]


puzzle.answer_a = sum(solve_a(line, False) for line in lines)
puzzle.answer_b = sum(solve_a(line, True) for line in lines)
