from aocd.models import Puzzle

puzzle = Puzzle(2019, int("01"))
data = puzzle.input_data
lines = data.splitlines()

a = 0
b = 0


for line in lines:
    fuel = int(line) // 3 - 2

    a += fuel

    while fuel > 0:
        b += fuel
        fuel = fuel // 3 - 2

puzzle.answer_a = a
puzzle.answer_b = b
