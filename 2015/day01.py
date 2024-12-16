from itertools import accumulate

from aocd.models import Puzzle

puzzle = Puzzle(2015, int("01"))
data = puzzle.input_data

puzzle.answer_a = sum(c == "(" for c in data) - sum(c == ")" for c in data)
puzzle.answer_b = list(accumulate([1 if c == "(" else -1 for c in data])).index(-1) + 1
