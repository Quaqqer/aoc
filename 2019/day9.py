import util
from aocd.models import Puzzle
from intpc import IntPC

puzzle = Puzzle(2019, int("09"))
data = puzzle.input_data
program = util.ints(data.split(","))


puzzle.answer_a = IntPC(program).io([1], to_halt=True)[-1]
puzzle.answer_b = IntPC(program).io([2], to_halt=True)[-1]
