import itertools
from collections import deque
from copy import copy
from enum import Enum
from typing import Deque, Iterable

from aocd.models import Puzzle

import util

puzzle = Puzzle(2019, int("07"))
data = puzzle.input_data
program = util.ints(data.split(","))


class State(Enum):
    NONE = 0
    HALTED = 1
    WAIT_FOR_INPUT = 2
    OUTPUT = 3


class IntPC:
    def __init__(self, program: list[int], input_queue: Iterable[int] = []):
        self.mem = copy(program)
        self.pc = 0
        self.state = State.NONE
        self.outputs: list[int] = []
        self.input_queue: Deque[int] = deque(input_queue)

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
                self.write(dest, a + b)
            case 2:
                [a, b, dest] = self.read_args(m1, m2, 1)
                self.write(dest, a * b)
            case 3:
                [a] = self.read_args(1)

                if len(self.input_queue) != 0:
                    read = self.input_queue.popleft()
                    self.write(a, read)
                else:
                    self.pc -= 2
                    self.state = State.WAIT_FOR_INPUT
            case 4:
                [a] = self.read_args(0)
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
                self.write(dest, 1 if a < b else 0)
            case 8:
                [a, b, dest] = self.read_args(m1, m2, 1)
                self.write(dest, 1 if a == b else 0)
            case 99:
                self.state = State.HALTED
            case unimplemented:
                raise Exception(f"Unimplemented opcode `{unimplemented}`")

    def read(self, i: int) -> int:
        return self.mem[i]

    def write(self, dest: int, v: int):
        self.mem[dest] = v

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
                case unimplemented:
                    raise Exception(f"Unimplemented read mode {unimplemented}")

        return vs

    def queue_inputs(self, *vs: int):
        self.input_queue += deque(vs)

        if self.state == State.WAIT_FOR_INPUT and len(self.input_queue) != 0:
            self.state = State.NONE


def run_a(phases: tuple[int, int, int, int, int]) -> int:
    assert len(phases) == 5
    v = 0
    for phase in phases:
        intpc = IntPC(program, [phase, v])
        intpc.run()
        [v] = intpc.outputs
    return v


puzzle.answer_a = max(
    run_a(phases) for phases in itertools.permutations([0, 1, 2, 3, 4, 5], 5)
)


def run_b(phases: tuple[int, int, int, int, int]) -> int:
    pcs = [IntPC(program) for _ in range(5)]

    for i, phase in enumerate(phases):
        pcs[i].queue_inputs(phase)

    v = 0
    e = 0

    while True:
        for i, intpc in enumerate(pcs):
            if intpc.state == State.HALTED:
                return e

            intpc.queue_inputs(v)
            intpc.run()
            v = intpc.outputs[-1]

            if intpc == pcs[-1]:
                e = v

            i = (i + 1) % 5


puzzle.answer_b = max(
    run_b(phases) for phases in itertools.permutations([5, 6, 7, 8, 9], 5)
)
