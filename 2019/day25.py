import itertools
import re
from collections import defaultdict, deque
from dataclasses import dataclass
from typing import Iterable

import util
from aocd.models import Puzzle
from intpc import IntPC, State

puzzle = Puzzle(2019, int("25"))
data = puzzle.input_data
program = util.ints(data.split(","))


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

            intpc.io(f"take {item}\n")
            items.append(item)

        for door in msg.doors:
            if door not in directions[msg.room]:
                output = intpc.io_str(f"{door}\n")
                next_msg = parse_enter(output)

                directions[msg.room][door] = next_msg.room
                directions[next_msg.room][opposites[door]] = msg.room

                inner(next_msg)

                output = intpc.io_str(f"{opposites[door]}\n")
                back_msg = parse_enter(output)
                assert back_msg.room == msg.room

    intpc = IntPC(program)
    msg = parse_enter(intpc.io_str([]))
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
        intpc.io_str(f"{dir}\n")


INTERACTIVE = False

intpc = IntPC(program)


def powerset[T](l: set[T]) -> Iterable[set[T]]:
    return (set(p) for i in range(len(l) + 1) for p in itertools.combinations(l, i))


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
            intpc.io(f"take {i}\n")

        for i in to_drop:
            intpc.io(f"drop {i}\n")

        output = intpc.io_str("north\n")

        if intpc.state == State.HALTED:
            return util.ints(output)[0]

    raise Exception("Didn't find the password")


puzzle.answer_a = find_password()
