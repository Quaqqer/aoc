# Solved both parts in 3:16:11

from collections import Counter

from aocd.models import Puzzle

puzzle = Puzzle(2018, int("2"))
data = puzzle.input_data
lines = data.splitlines()

doubles = sum(2 in Counter(line).values() for line in lines)
triples = sum(3 in Counter(line).values() for line in lines)
puzzle.answer_a = doubles * triples


def solve_b():
    for i in range(len(lines)):
        for j in range(i + 1, len(lines)):
            a, b = lines[i], lines[j]

            diffs = sum(ac != bc for ac, bc in zip(a, b))

            if diffs == 1:
                return "".join(ac for ac, bc in zip(a, b) if ac == bc)

    raise Exception("No solution")


puzzle.answer_b = solve_b()
