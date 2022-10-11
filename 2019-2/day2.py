#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2019, 2)
lines = puzzle.input_data.split("\n")


# Main code
class IntcodePC:
    def __init__(self, program: list[int]):
        self.program = program
        self.pc: int = 0

    def go(self, n):
        old_pc = self.pc
        self.pc += n
        return self.program[old_pc : self.pc]

    def read(self, n) -> int:
        return self.program[n]

    def set(self, v, n):
        self.program[n] = v

    def run(self):
        while True:
            match self.program[self.pc]:
                case 1:
                    _, laddr, raddr, sumaddr = self.go(4)
                    self.set(self.read(laddr) + self.read(raddr), sumaddr)
                case 2:
                    _, laddr, raddr, sumaddr = self.go(4)
                    self.set(self.read(laddr) * self.read(raddr), sumaddr)
                case 99:
                    return
                case _:
                    raise ValueError("Unknown op-code")


program = [int(v) for v in lines[0].split(",")]


def run(noun, verb, program) -> int:
    prg = program.copy()
    prg[1] = noun
    prg[2] = verb
    pc = IntcodePC(prg)
    pc.run()
    return pc.program[0]


silver = run(12, 2, program)

for noun in range(100):
    for verb in range(100):
        if run(noun, verb, program) == 19690720:
            gold = 100 * noun + verb


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
