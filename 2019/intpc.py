from collections import deque
from copy import copy
from enum import Enum
from typing import Iterable


class State(Enum):
    NONE = 0
    HALTED = 1
    WAIT_FOR_INPUT = 2


def to_ascii(l: list[int]) -> str:
    return "".join(chr(c) for c in l)


class IntPC:
    def __init__(self, program: list[int], input: Iterable[int] | str | None = None):
        """Create an IntPC instance

        Args:
            program: The program code to execute
        """
        self.mem = copy(program) + [0] * (10**6 - len(program))
        self.pc = 0
        self.state = State.NONE
        self.outputs: list[int] = []
        self.input_queue: deque[int] = deque()
        self.relative_base = 0

        if input is not None:
            self.queue_input(input)

    def _tick(self):
        """Execute a single instruction"""
        [op] = self._read_args(1)

        opcode = op % 100

        m1 = (op // 100) % 10
        m2 = (op // 1000) % 10
        m3 = (op // 10000) % 10

        match opcode:
            case 1:
                [a, b, dest] = self._read_args(m1, m2, 1)
                self._write(dest, a + b, m3)
            case 2:
                [a, b, dest] = self._read_args(m1, m2, 1)
                self._write(dest, a * b, m3)
            case 3:
                [a] = self._read_args(1)

                if len(self.input_queue) != 0:
                    read = self.input_queue.popleft()
                    self._write(a, read, m1)
                else:
                    self.pc -= 2
                    self.state = State.WAIT_FOR_INPUT
            case 4:
                [a] = self._read_args(m1)
                self.outputs.append(a)
            case 5:
                [cond, jmp] = self._read_args(m1, m2)
                if cond != 0:
                    self.pc = jmp
            case 6:
                [cond, jmp] = self._read_args(m1, m2)
                if cond == 0:
                    self.pc = jmp
            case 7:
                [a, b, dest] = self._read_args(m1, m2, 1)
                self._write(dest, 1 if a < b else 0, m3)
            case 8:
                [a, b, dest] = self._read_args(m1, m2, 1)
                self._write(dest, 1 if a == b else 0, m3)
            case 9:
                [a] = self._read_args(m1)
                self.relative_base += a
            case 99:
                self.state = State.HALTED
            case unimplemented:
                raise Exception(f"Unimplemented opcode `{unimplemented}`")

    def _read(self, i: int) -> int:
        """Read from the memory

        Args:
            i: The memory index

        Returns:
            The read value
        """
        return self.mem[i]

    def _write(self, dest: int, v: int, mode: int):
        """Write to the memory

        Args:
            dest: The destionation
            v: The value to write
            mode: The mode of the destionation (0 => indirect, 2 => relative)
        """
        match mode:
            case 0:
                self.mem[dest] = v
            case 2:
                self.mem[self.relative_base + dest] = v
            case unimplemented:
                raise Exception(f"Unimplemented write mode {unimplemented}")

    def _read_args(self, *modes: int) -> list[int]:
        """Reads arguments (the memory at the program counter).

        Args:
            modes: The modes of the reads, 0 is indirect, 1 is immediate

        Returns:
            The read value
        """

        vs: list[int] = []

        for mode in modes:
            v = self._read(self.pc)
            self.pc += 1
            match mode:
                case 0:
                    vs.append(self._read(v))
                case 1:
                    vs.append(v)
                case 2:
                    vs.append(self._read(self.relative_base + v))
                case unimplemented:
                    raise Exception(f"Unimplemented read mode {unimplemented}")

        return vs

    def run(self, input: str | Iterable[int] | None = None, to_halt=False):
        """Run the intpc until it waits for input or is halted

        Args:
            input: Optional input
            to_halt: If the program should halt after running
        """
        if input is not None:
            self.queue_input(input)

        while self.state == State.NONE:
            self._tick()

        if to_halt and self.state != State.HALTED:
            raise Exception("Expected to run to halt")

    def io(self, input: str | Iterable[int], to_halt=False) -> list[int]:
        """Run the intpc until it waits for input or is halted and get the output

        Args:
            input: Optional input
            to_halt: If the program should halt after running

        Returns:
            The output
        """
        self.run(input, to_halt=to_halt)

        return self.consume_outputs()

    def io_str(self, input: str | Iterable[int], to_halt=False) -> str:
        """Run the intpc until it waits for input or is halted and get the output as an ascii string

        Args:
            input: Optional input
            to_halt: If the program should halt after running

        Returns:
            The output as a string
        """
        return to_ascii(self.io(input, to_halt=to_halt))

    def queue_input(self, input: Iterable[int] | str):
        """Queue input

        Args:
            input: The input, either ascii string or integers
        """
        match input:
            case str(s):
                self.input_queue += deque((ord(c) for c in s))
            case input:
                self.input_queue += deque(input)

        if self.state == State.WAIT_FOR_INPUT and len(self.input_queue) != 0:
            self.state = State.NONE

    def consume_outputs(self) -> list[int]:
        """Return the output buffer and clear it

        Returns:
            The output buffer
        """
        outs = self.outputs
        self.outputs = []
        return outs

    def consume_ascii_output(self) -> str:
        """Return the output buffer as a string and clear it

        Returns:
            The output buffer as an ascii string
        """
        return "".join(chr(c) for c in self.consume_outputs())

    def clear_outputs(self):
        """Clear the outputs"""
        self.outputs = []

    @property
    def is_halted(self) -> bool:
        return self.state == State.HALTED
