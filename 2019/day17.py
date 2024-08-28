from collections import defaultdict, deque
from copy import copy
from enum import Enum
from typing import Deque, Iterable

import util
from aocd.models import Puzzle
from util import Grid, Vec2

puzzle = Puzzle(2019, int("17"))
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


dirs = [Vec2(0, -1), Vec2(0, 1), Vec2(-1, 0), Vec2(1, 0)]


def discover_map(pos: Vec2):
    opposite = [1, 0, 3, 2]

    map: dict[Vec2, str] = defaultdict(lambda: "?", {Vec2(0, 0): " "})

    intpc = IntPC(program)

    def move(dir: int) -> int:
        intpc.queue_inputs(dir + 1)
        intpc.run()
        [output] = intpc.consume_outputs()
        return output

    def inner(pos: Vec2):
        for i, dir in enumerate(dirs):
            if map[pos + dir] == "?":
                output = move(i)

                match output:
                    case 0:
                        map[pos + dir] = "#"
                    case 1:
                        map[pos + dir] = " "
                        inner(pos + dir)
                        move(opposite[i])
                    case 2:
                        map[pos + dir] = "O"
                        inner(pos + dir)
                        move(opposite[i])

    inner(pos)

    return map


def render(outputs: list[int]) -> Grid[str]:
    screen: list[str] = []
    screen = [chr(code) for code in outputs]
    screen.pop()
    screen.pop()

    return Grid.from_lines("".join(screen))


intpc = IntPC(program)
intpc.run()
g = render(intpc.outputs)


puzzle.answer_a = sum(
    x * y
    for x, y in g.find(
        lambda pos, _: g[pos] == "#" and all(g[n] == "#" for n in g.neighbours4(pos))
    )
)


[start] = g.find_value("^")
delta = Vec2(0, -1)
pos = Vec2.from_tuple(start)

path: tuple[str, ...] = ()

assert g[(pos + delta).to_tuple()] != "#"

while True:
    if (right := g.get(*(pos + delta.rot_r()).to_tuple())) == "#":
        turn = "R"
        delta = delta.rot_r()
    elif (left := g.get(*(pos + delta.rot_l()).to_tuple())) == "#":
        turn = "L"
        delta = delta.rot_l()
    else:
        break

    moved = 0
    while (next := g.get(*(pos + delta).to_tuple())) == "#":
        moved += 1
        pos += delta

    path += (f"{turn},{moved}",)


def find_abc(
    path: tuple[str, ...],
    abc: tuple[tuple[str, ...], ...] = (),
    history: tuple[str, ...] = (),
) -> tuple[tuple[str, ...], str] | None:
    if len(",".join(history)) > 20:
        return None

    if path == ():
        return tuple(",".join(s) for s in abc), ",".join(history)

    for i, part in enumerate(abc):
        if path[: len(part)] == part:
            ans = find_abc(path[len(part) :], abc, history + (chr(ord("A") + i),))

            if ans is not None:
                return ans

    if len(abc) < 3:
        for i in range(1, len(path)):
            part = path[:i]

            if len(",".join(part)) > 20:
                break

            ans = find_abc(path, abc + (part,), history)

            if ans is not None:
                return ans


ans = find_abc(path)
assert ans is not None
(a, b, c), main = ans


intpc = IntPC(program)
intpc.mem[0] = 2

for p in [main, a, b, c]:
    intpc.queue_inputs(*map(ord, p), ord("\n"))

intpc.queue_inputs(ord("n"), ord("\n"))

intpc.run()

puzzle.answer_b = intpc.outputs[-1]
