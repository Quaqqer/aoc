from copy import copy

from aocd.models import Puzzle

from util import ints

puzzle = Puzzle(2019, int("02"))
data = puzzle.input_data

program = ints(data)


class IntPC:
    def __init__(self, program: list[int]):
        self.pc = 0
        self.mem = copy(program)

    def advance(self, n: int) -> list[int]:
        vs = self.mem[self.pc : self.pc + n]
        self.pc += n
        return vs

    def read(self, i: int) -> int:
        return self.mem[i]

    def write(self, i: int, v: int):
        self.mem[i] = v

    def run(self):
        while True:
            op = self.mem[self.pc]

            match op:
                case 1:
                    _, a, b, dest = self.advance(4)
                    self.write(dest, self.read(a) + self.read(b))
                case 2:
                    _, a, b, dest = self.advance(4)
                    self.write(dest, self.read(a) * self.read(b))
                case 99:
                    break
                case unimplemented:
                    raise Exception(f"Unimplemented opcode {unimplemented}")


def eval_input(a: int, b: int) -> int:
    intpc = IntPC([program[0], a, b, *program[3:]])
    intpc.run()
    return intpc.mem[0]


puzzle.answer_a = eval_input(12, 2)

[(noun, verb)] = [
    (a, b) for a in range(100) for b in range(100) if eval_input(a, b) == 19690720
]
puzzle.answer_b = noun * 100 + verb
