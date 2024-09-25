from collections import deque
from copy import copy
from enum import Enum
from typing import Deque, Iterable

import util
from aocd.models import Puzzle

puzzle = Puzzle(2019, int("23"))
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

    def consume_outputs(self) -> list[int]:
        outs = self.outputs
        self.clear_outputs()
        return outs

    def clear_outputs(self):
        self.outputs = []


def solve_a() -> int:
    pcs = [IntPC(program, [i]) for i in range(50)]

    packets: dict[int, deque[tuple[int, int]]] = {i: deque() for i in range(50)}

    while True:
        for i in range(50):
            pc = pcs[i]
            input = None if len(packets[i]) == 0 else packets[i].popleft()

            if input is None:
                pc.queue_inputs(-1)
            else:
                pc.queue_inputs(*input)
            pc.run()

            for to, x, y in util.chunk(pc.consume_outputs(), 3):
                if to == 255:
                    return y

                packets[to].append((x, y))


def solve_b() -> int:
    pcs = [IntPC(program, [i]) for i in range(50)]

    packets: dict[int, deque[tuple[int, int]]] = {i: deque() for i in range(50)}

    nat_x, nat_y = -1, -1

    sent_ys: set[int] = set()

    while True:
        idle = all(len(q) == 0 for q in packets.values())

        if idle:
            packets[0].append((nat_x, nat_y))

            if nat_y in sent_ys:
                return nat_y

            sent_ys |= {nat_y}

        for i in range(50):
            pc = pcs[i]
            input = None if len(packets[i]) == 0 else packets[i].popleft()

            if input is None:
                pc.queue_inputs(-1)
            else:
                pc.queue_inputs(*input)
            pc.run()

            for to, x, y in util.chunk(pc.consume_outputs(), 3):
                if to == 255:
                    nat_x, nat_y = x, y
                else:
                    packets[to].append((x, y))


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
