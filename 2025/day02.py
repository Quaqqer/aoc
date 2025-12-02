# pyright: basic

from aocd.models import Puzzle

puzzle = Puzzle(2025, int("02"))


def is_invalid_a(id: str):
    if len(id) % 2 == 1:
        return False
    m = len(id) // 2
    return id[:m] == id[m:]


def is_invalid_b(id: str):
    for j in range(1, len(id)):
        if id == id[:j] * (len(id) // (j)):
            return True
    return False


def solve(input: str, part_b: bool):
    s = 0

    for l in input.split(","):
        a, b = l.split("-")
        a, b = int(a), int(b)

        for i in range(a, b + 1):
            if not part_b and is_invalid_a(str(i)):
                s += i
            elif part_b and is_invalid_b(str(i)):
                s += i

    return s


puzzle.answer_a = solve(puzzle.input_data, False)
puzzle.answer_b = solve(puzzle.input_data, True)
