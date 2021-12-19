#!/usr/bin/env python3
import heapq
from dataclasses import dataclass, field

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 15)
lines = puzzle.input_data.split("\n")


# Main code
cave = list(map(list, zip(*[[int(risk) for risk in line] for line in lines])))


def neighbours(pos):
    x, y = pos
    yield x + 1, y
    yield x - 1, y
    yield x, y + 1
    yield x, y - 1


def in_cave(pos, cave, repeated: int):
    x, y = pos
    return 0 <= x < len(cave) * repeated and 0 <= y < len(cave[0]) * repeated


@dataclass(order=True)
class P:
    cost: int
    pos: tuple[int, int] = field(compare=False)


def get_cost(pos, cave, repeated: int) -> int:
    x, y = pos
    if repeated == 1:
        return cave[x][y]
    else:
        mx = x % len(cave)
        my = y % len(cave[0])
        ax = x // len(cave)
        ay = y // len(cave[0])
        new_cost = ax + ay + cave[mx][my]
        while new_cost >= 10:
            new_cost -= 9
        return new_cost


def lowest_risk(cave, initial_pos, goal, repeated: int) -> int:
    queue = [P(cost=0, pos=initial_pos)]
    visited = set(initial_pos)

    while len(queue) > 0:
        elem = heapq.heappop(queue)
        pos = elem.pos
        cost = elem.cost

        if pos == goal:
            return cost

        for n in neighbours(pos):
            if in_cave(n, cave, repeated) and n not in visited:
                nelem = P(cost=cost + get_cost(n, cave, repeated), pos=n)
                visited.add(nelem.pos)
                heapq.heappush(queue, nelem)
    return -1


silver_goal = (len(cave) - 1, len(cave[0]) - 1)
silver = lowest_risk(cave, (0, 0), silver_goal, 1)

gold_goal = (len(cave) * 5 - 1, len(cave[0]) * 5 - 1)
gold = lowest_risk(cave, (0, 0), gold_goal, 5)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    # puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
