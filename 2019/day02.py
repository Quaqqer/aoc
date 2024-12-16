from aocd.models import Puzzle
from intpc import IntPC
from util import ints

puzzle = Puzzle(2019, int("02"))
data = puzzle.input_data

program = ints(data)


def eval_input(a: int, b: int) -> int:
    intpc = IntPC([program[0], a, b, *program[3:]])
    intpc.run()
    return intpc.mem[0]


puzzle.answer_a = eval_input(12, 2)

[(noun, verb)] = [
    (a, b) for a in range(100) for b in range(100) if eval_input(a, b) == 19690720
]
puzzle.answer_b = noun * 100 + verb
