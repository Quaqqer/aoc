#!/usr/bin/env python3
import itertools

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2019, 7)
lines = puzzle.input_data.split("\n")


# Main code
class IntcodePC:
    def __init__(self, program: list[int], mocked_stdin=[], debug=False):
        self.memory = program
        self.pc: int = 0
        self.debug = debug
        self.mocked_stdin_r = mocked_stdin[::-1]

    def go1(self) -> int:
        return self.go(1)[0]

    def go(self, n) -> list[int]:
        old_pc = self.pc
        self.pc += n
        return self.memory[old_pc : self.pc]

    def read(self, n, immediate: bool = False) -> int:
        return n if immediate else self.memory[n]

    def write(self, v, n):
        self.memory[n] = v

    def read_op(self) -> tuple[int, bool, bool, bool]:
        op = self.go1()
        opcode = op % 100
        im1 = (op // 100) % 10 == 1
        im2 = (op // 1000) % 10 == 1
        im3 = (op // 10000) % 10 == 1
        return opcode, im1, im2, im3

    def run_stdout(self):
        out = self.run()
        map(print, out)

    def run(self) -> list[int]:
        output = []

        while self.pc < len(self.memory):
            if self.debug:
                print(
                    [
                        m if i != self.pc else f"pc: {m}"
                        for i, m in enumerate(self.memory)
                    ]
                )

            opcode, im1, im2, im3 = self.read_op()

            match opcode:
                case 1:
                    laddr, raddr, sumaddr = self.go(3)
                    self.write(self.read(laddr, im1) + self.read(raddr, im2), sumaddr)
                case 2:
                    laddr, raddr, sumaddr = self.go(3)
                    self.write(self.read(laddr, im1) * self.read(raddr, im2), sumaddr)
                case 3:
                    print(self.mocked_stdin_r)
                    if self.mocked_stdin_r:
                        read = self.mocked_stdin_r.pop()
                    else:
                        read = int(input())

                    write_addr = self.go1()
                    self.write(read, write_addr)
                case 4:
                    addr = self.go1()
                    out = self.read(addr, im1)
                    output.append(out)
                case 5:
                    cond, addr = self.go(2)
                    if self.read(cond, im1) != 0:
                        self.pc = self.read(addr, im2)
                case 6:
                    cond, addr = self.go(2)
                    if self.read(cond, im1) == 0:
                        self.pc = self.read(addr, im2)
                case 7:
                    lhs, rhs, addr = self.go(3)
                    self.write(int(self.read(lhs, im1) < self.read(rhs, im2)), addr)
                case 8:
                    lhs, rhs, addr = self.go(3)
                    self.write(int(self.read(lhs, im1) == self.read(rhs, im2)), addr)
                case 99:
                    return output
                case _:
                    raise ValueError(f"Unknown op-code: {opcode}")

        raise Exception("Exited without an exit operation")


program = [int(v) for v in lines[0].split(",")]

vs = []
for perm in itertools.permutations(range(5)):
    v = 0
    for i in range(5):
        print([perm[i], v])
        ipc = IntcodePC(program.copy(), mocked_stdin=[perm[i], v])
        [v] = ipc.run()
    vs.append(v)
silver = max(vs)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
