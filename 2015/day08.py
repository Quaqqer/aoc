import json

from aocd.models import Puzzle

puzzle = Puzzle(2015, int("8"))
data = puzzle.input_data
lines = data.splitlines()

puzzle.answer_a = sum(len(line) - len(eval(line)) for line in lines)
puzzle.answer_b = sum(len(json.dumps(line)) - len(line) for line in lines)
