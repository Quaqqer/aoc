# 11   04:29:00  15994      0   04:34:30  10996      0
# overslept by a few hours today

import math
import re
from dataclasses import dataclass

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("11"))
id = puzzle.input_data


@dataclass
class Monkey:
    items: list[int]
    operation: str
    divider: int
    true: int
    false: int


def subplay(monkey: Monkey, worry: int, part_b: bool, mod: int) -> tuple[int, int]:
    op_result = eval(monkey.operation.replace("old", str(worry)))
    new_worry = op_result % mod if part_b else op_result // 3
    return (monkey.true if new_worry % monkey.divider == 0 else monkey.false, new_worry)


def play(monkey: Monkey, part_b: bool, mod: int) -> list[tuple[int, int]]:
    thrown = [subplay(monkey, worry, part_b, mod) for worry in monkey.items]
    monkey.items = []
    return thrown


def parse_monkey(chunk: str):
    lines = chunk.splitlines()

    items = list(map(int, re.findall(r"\d+", lines[1])))
    operation, *_ = re.findall(r"old (?:\+|\*) (?:\d+|old)", lines[2])
    divider, true, false = map(int, re.findall(r"\d+", "\n".join(lines[3:])))

    return Monkey(items, operation, divider, true, false)


def solve(n: int, part_b: bool) -> int:
    monkeys = [parse_monkey(chunk) for chunk in id.split("\n\n")]
    inspections = [0 for _ in range(len(monkeys))]
    mod = math.lcm(*[monkey.divider for monkey in monkeys])

    for _ in range(n):
        for i, monkey in enumerate(monkeys):
            thrown = play(monkey, part_b, mod)

            inspections[i] += len(thrown)

            for throw_to_monkey, item_worry in thrown:
                monkeys[throw_to_monkey].items.append(item_worry)

    *_, second_busiest_monkey, busiest_monkey = sorted(inspections)
    return second_busiest_monkey * busiest_monkey


puzzle.answer_a = solve(20, False)
puzzle.answer_b = solve(10000, True)
