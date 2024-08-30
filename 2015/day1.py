from aocd.models import Puzzle

puzzle = Puzzle(2015, int("01"))
data = puzzle.input_data

puzzle.answer_a = sum(c == "(" for c in data) - sum(c == ")" for c in data)


def find_b() -> int:
    s = 0
    for i, c in enumerate(data):
        if c == "(":
            s += 1
        if c == ")":
            s -= 1
        if s == -1:
            return i + 1
    raise Exception("No solution")


puzzle.answer_b = find_b()
