# 6   00:03:30   00:27:01
# Slow part 2 today :P

# pyright: basic

from aocd.models import Puzzle

puzzle = Puzzle(2025, int("06"))


def solve_a(input: str):
    *lines, ops_line = input.splitlines()
    rows = [list(map(int, line.split())) for line in lines]
    ops = [c for c in ops_line if c != " "]

    tot = 0
    for col_i, op in enumerate(ops):
        if op == "+":
            for row in rows:
                tot += row[col_i]
        if op == "*":
            prod = 1
            for row in rows:
                prod *= row[col_i]
            tot += prod
    return tot


def solve_b(input: str):
    *lines, ops_line = input.splitlines()

    ops_is = [(c, i) for i, c in enumerate(ops_line) if c != " "]
    splits: list[int | None] = tuple(zip(*ops_is))[1] + (None,)  # pyright: ignore[reportAssignmentType]
    rows = [[line[a:b] for a, b in zip(splits, splits[1:])] for line in lines]
    cols = tuple(zip(*rows))

    tot = 0

    for col_i, (op, _) in enumerate(ops_is):
        sum_prod = 0 if op == "+" else 1

        for ss in zip(*cols[col_i]):
            ss = "".join(ss).replace(" ", "")
            if ss != "":
                num = int(ss)
                sum_prod = sum_prod + num if op == "+" else sum_prod * num
        tot += sum_prod

    return tot


puzzle.answer_a = solve_a(puzzle.input_data)
puzzle.answer_b = solve_b(puzzle.input_data)
