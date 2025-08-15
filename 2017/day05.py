from aocd.models import Puzzle

puzzle = Puzzle(2017, int("05"))

jumps = list(map(int, puzzle.input_data.splitlines()))


def solve_a(jumps: list[int]):
    n_jumps = 0
    i = 0

    while 0 <= i < len(jumps):
        jumps[i] += 1
        i += jumps[i] - 1
        n_jumps += 1

    return n_jumps


def solve_b(jumps: list[int]):
    n_jumps = 0
    i = 0

    while 0 <= i < len(jumps):
        if jumps[i] >= 3:
            jumps[i] -= 1
            i += jumps[i] + 1
        else:
            jumps[i] += 1
            i += jumps[i] - 1
        n_jumps += 1

    return n_jumps


puzzle.answer_a = solve_a(jumps.copy())
puzzle.answer_b = solve_b(jumps.copy())
