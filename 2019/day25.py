import itertools
import re
from collections import defaultdict, deque
from copy import copy
from dataclasses import dataclass
from enum import Enum
from typing import Deque, Iterable

import util
from aocd.models import Puzzle

puzzle = Puzzle(2019, int("25"))
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

    def queue_ascii_input(self, s: str):
        self.queue_inputs(*(ord(c) for c in s))

    def consume_outputs(self) -> list[int]:
        outs = self.outputs
        self.clear_outputs()
        return outs

    def consume_ascii_output(self) -> str:
        return "".join(chr(c) for c in self.consume_outputs())

    def clear_outputs(self):
        self.outputs = []


@dataclass
class EnterMessage:
    room: str
    doors: list[str]
    items: list[str]
    status: str


def parse_enter(s: str) -> EnterMessage:
    groups = [group.splitlines() for group in s.strip().split("\n\n")]
    groups = [g for g in groups if g != []]

    [*_, name_group] = [g for g in groups if g[0].startswith("== ")]
    m = re.match(r"== (.*) ==", name_group[0])
    assert m
    room = m.group(1)
    status = name_group[1]

    [doors_group] = [g for g in groups if g[0] == "Doors here lead:"]
    doors = [l[2:] for l in doors_group[1:]]

    items_groups = [g for g in groups if g[0] == "Items here:"]
    match items_groups:
        case [items_group]:
            items = [l[2:] for l in items_group[1:]]
        case _:
            items = []

    return EnterMessage(room=room, doors=doors, items=items, status=status)


ITEM_BLACKLIST = {
    "infinite loop",
    "giant electromagnet",
    "escape pod",
    "photons",
    "molten lava",
}

opposites = {"north": "south", "south": "north", "east": "west", "west": "east"}


def explore_and_collect() -> tuple[IntPC, dict[str, dict[str, str]], str, list[str]]:
    directions: dict[str, dict[str, str]] = defaultdict(dict)
    items: list[str] = []

    def inner(msg: EnterMessage):
        # Don't explore security checkpoint
        if msg.room == "Security Checkpoint":
            return

        for item in msg.items:
            if item in ITEM_BLACKLIST:
                continue

            intpc.queue_ascii_input(f"take {item}\n")
            intpc.run()
            items.append(item)

        for door in msg.doors:
            if door not in directions[msg.room]:
                intpc.queue_ascii_input(f"{door}\n")
                intpc.run()
                output = intpc.consume_ascii_output()
                next_msg = parse_enter(output)

                directions[msg.room][door] = next_msg.room
                directions[next_msg.room][opposites[door]] = msg.room

                inner(next_msg)

                intpc.queue_ascii_input(f"{opposites[door]}\n")
                intpc.run()
                output = intpc.consume_ascii_output()
                back_msg = parse_enter(output)
                assert back_msg.room == msg.room

    intpc = IntPC(program)
    intpc.run()
    msg = parse_enter(intpc.consume_ascii_output())
    inner(msg)

    return intpc, directions, msg.room, items


def find_path(directions: dict[str, dict[str, str]], from_: str, to: str) -> list[str]:
    visited = set()

    q: deque[tuple[str, list[str]]] = deque([(from_, [])])

    while q:
        pos, path = q.popleft()

        if pos == to:
            return path

        for direction, room in directions[pos].items():
            if room not in visited:
                q.append((room, path + [direction]))
                visited.add(room)

    raise Exception("No path found")


def goto(intpc: IntPC, directions: dict[str, dict[str, str]], from_: str, to: str):
    path = find_path(directions, from_, to)

    for dir in path:
        intpc.queue_ascii_input(f"{dir}\n")
        intpc.run()
        intpc.consume_outputs()


INTERACTIVE = False

intpc = IntPC(program)


def powerset[T](l: set[T]) -> Iterable[set[T]]:
    yield set()
    for i in range(1, len(l)):
        yield from (set(p) for p in itertools.combinations(l, i))


def find_password() -> int:
    intpc, directions, position, items = explore_and_collect()

    goto(intpc, directions, position, "Security Checkpoint")

    items = set(items)
    inventory = set(items)

    for p in powerset(items):
        to_pickup = p - inventory
        to_drop = inventory - p
        inventory = p

        for i in to_pickup:
            intpc.queue_ascii_input(f"take {i}\n")

        for i in to_drop:
            intpc.queue_ascii_input(f"drop {i}\n")

        intpc.run()
        intpc.consume_ascii_output()

        intpc.queue_ascii_input("north\n")
        intpc.run()
        output = intpc.consume_ascii_output()

        if intpc.state == State.HALTED:
            return util.ints(output)[0]

    raise Exception("Didn't find the password")


puzzle.answer_a = find_password()
