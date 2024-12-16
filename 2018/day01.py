from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2018, int("1"))
data = puzzle.input_data

deltas = ints(data)

puzzle.answer_a = sum(deltas)


def solve_b():
    seen = set()
    freq = 0
    i = 0

    while True:
        if freq in seen:
            return freq

        seen |= {freq}

        freq += deltas[i % len(deltas)]
        i += 1


puzzle.answer_b = solve_b()
