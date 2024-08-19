from collections import deque
from copy import copy
from enum import Enum
from typing import Deque, Iterable

from aocd.models import Puzzle

import util

puzzle = Puzzle(2019, int("11"))
data = puzzle.input_data
program = util.ints(data.split(","))


class State(Enum):
    NONE = 0
    HALTED = 1
    WAIT_FOR_INPUT = 2
    OUTPUT = 3


class IntPC:
    def __init__(self, program: list[int], input_queue: Iterable[int] = []):
        self.mem = copy(program) + [0] * (10**6 - len(program))
        self.pc = 0
        self.state = State.NONE
        self.outputs: list[int] = []
        self.input_queue: Deque[int] = deque(input_queue)
        self.relative_base = 0

    def run(self, continue_on_output=True) -> State:
        while self.state == State.NONE or (
            continue_on_output and self.state == State.OUTPUT
        ):
            if self.state == State.OUTPUT:
                self.state = State.NONE

            self.tick()

        return self.state

    def tick(self):
        [op] = self.read_args(1)

        opcode = op % 100

        m1 = (op // 100) % 10
        m2 = (op // 1000) % 10
        m3 = (op // 10000) % 10

        match opcode:
            case 1:
                [a, b, dest] = self.read_args(m1, m2, 1)
                self.write(dest, a + b, m3)
            case 2:
                [a, b, dest] = self.read_args(m1, m2, 1)
                self.write(dest, a * b, m3)
            case 3:
                [a] = self.read_args(1)

                if len(self.input_queue) != 0:
                    read = self.input_queue.popleft()
                    self.write(a, read, m1)
                else:
                    self.pc -= 2
                    self.state = State.WAIT_FOR_INPUT
            case 4:
                [a] = self.read_args(m1)
                self.outputs.append(a)
                self.state = State.OUTPUT
            case 5:
                [cond, jmp] = self.read_args(m1, m2)
                if cond != 0:
                    self.pc = jmp
            case 6:
                [cond, jmp] = self.read_args(m1, m2)
                if cond == 0:
                    self.pc = jmp
            case 7:
                [a, b, dest] = self.read_args(m1, m2, 1)
                self.write(dest, 1 if a < b else 0, m3)
            case 8:
                [a, b, dest] = self.read_args(m1, m2, 1)
                self.write(dest, 1 if a == b else 0, m3)
            case 9:
                [a] = self.read_args(m1)
                self.relative_base += a
            case 99:
                self.state = State.HALTED
            case unimplemented:
                raise Exception(f"Unimplemented opcode `{unimplemented}`")

    def read(self, i: int) -> int:
        return self.mem[i]

    def write(self, dest: int, v: int, mode: int):
        match mode:
            case 0:
                self.mem[dest] = v
            case 2:
                self.mem[self.relative_base + dest] = v
            case unimplemented:
                raise Exception(f"Unimplemented write mode {unimplemented}")

    def read_args(self, *modes: int) -> list[int]:
        """Reads arguments (the memory at the program counter).

        Args:
            modes: The modes of the reads, 0 is indirect, 1 is immediate

        Returns:
            The read value
        """

        vs: list[int] = []

        for mode in modes:
            v = self.read(self.pc)
            self.pc += 1
            match mode:
                case 0:
                    vs.append(self.read(v))
                case 1:
                    vs.append(v)
                case 2:
                    vs.append(self.read(self.relative_base + v))
                case unimplemented:
                    raise Exception(f"Unimplemented read mode {unimplemented}")

        return vs

    def queue_inputs(self, *vs: int):
        self.input_queue += deque(vs)

        if self.state == State.WAIT_FOR_INPUT and len(self.input_queue) != 0:
            self.state = State.NONE


x, y = 0, 0
dx, dy = 0, -1
painted = set()
whites = set()


def rot_r():
    global dx, dy
    dx, dy = -dy, dx


def rot_l():
    global dx, dy
    dx, dy = dy, -dx


intpc = IntPC(program)
while True:
    intpc.queue_inputs(1 if (x, y) in whites else 0)
    intpc.run()

    if intpc.state == State.HALTED:
        break

    *_, paint, turn = intpc.outputs
    painted.add((x, y))
    if paint == 1:
        whites.add((x, y))
    else:
        if (x, y) in whites:
            whites.remove((x, y))

    if turn == 1:
        rot_r()
    else:
        rot_l()

    x, y = x + dx, y + dy

puzzle.answer_a = len(painted)

x, y = 0, 0
dx, dy = 0, -1
whites = {(0, 0)}
painted = set()

intpc = IntPC(program)
while True:
    intpc.queue_inputs(1 if (x, y) in whites else 0)
    intpc.run()

    if intpc.state == State.HALTED:
        break

    *_, paint, turn = intpc.outputs
    painted.add((x, y))
    if paint == 1:
        whites.add((x, y))
    else:
        if (x, y) in whites:
            whites.remove((x, y))

    if turn == 1:
        rot_r()
    else:
        rot_l()

    x, y = x + dx, y + dy


util.print_set_grid(whites)

print("What letters do you see?")
ans = input("> ")
assert len(ans) == 8
puzzle.answer_b = ans
