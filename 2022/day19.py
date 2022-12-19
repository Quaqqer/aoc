# 19   01:01:24    284      0   01:46:45    622      0
# bruteforce chads, we can't stop winning!

import functools
import math
import re
from heapq import heappop, heappush
from multiprocessing import Pool

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("19"))
id = puzzle.input_data


blueprints = []
for line in id.splitlines():
    i, ore_ore, clay_ore, obsidian_ore, obsidian_clay, geode_ore, geode_obsidian = map(
        int,
        re.match(
            r"Blueprint (\d+): "
            + r"Each ore robot costs (\d+) ore. "
            + r"Each clay robot costs (\d+) ore. "
            + r"Each obsidian robot costs (\d+) ore and (\d+) clay. "
            + r"Each geode robot costs (\d+) ore and (\d+) obsidian.",
            line,
        ).groups(),  # type: ignore
    )

    blueprints.append(
        (
            (ore_ore, 0, 0, 0),
            (clay_ore, 0, 0, 0),
            (obsidian_ore, obsidian_clay, 0, 0),
            (geode_ore, 0, geode_obsidian, 0),
        )
    )


State = tuple[tuple[int, int, int, int], tuple[int, int, int, int], int]


@functools.cache
def heuristic(state: State, blueprint, max_time: int) -> int:
    def inner(state) -> int:
        robots, resources, time = state
        if time == max_time:
            return resources[3]
        else:
            added_robots = [0] * 4
            for r in range(4):
                if all(blueprint[r][i] <= resources[i] for i in range(4)):
                    added_robots[r] = 1
            new_resources = lib.tup_add(resources, robots)
            return inner(
                (lib.tup_add(robots, tuple(added_robots)), new_resources, time + 1)
            )

    return inner(state)


def search_a(blueprint, max_time: int) -> int:
    q = []
    visited = set()
    initial_state = ((1, 0, 0, 0), (0, 0, 0, 0), 0)
    q.append((-heuristic(initial_state, blueprint, max_time), initial_state))
    visited.add(initial_state)

    while q:
        _nheur, (robots, resources, time) = heappop(q)
        gained_resources = robots

        if time == max_time:
            return resources[3]

        for build_robot in range(4):
            cost = blueprint[build_robot]
            new_resources = lib.tup_sub(resources, cost)

            if all(new_r >= 0 for new_r in new_resources):
                gained_robot = [0, 0, 0, 0]
                gained_robot[build_robot] = 1
                new_robots = lib.tup_add(robots, tuple(gained_robot))

                next_state = (
                    new_robots,
                    lib.tup_add(new_resources, gained_resources),
                    time + 1,
                )
                if next_state not in visited:
                    heappush(
                        q, (-heuristic(next_state, blueprint, max_time), next_state)
                    )
                    visited.add(next_state)

        next_state = (robots, lib.tup_add(resources, gained_resources), time + 1)
        if next_state not in visited:
            heappush(q, (-heuristic(next_state, blueprint, max_time), next_state))
            visited.add(next_state)

    raise Exception()


def lambda_a(args):
    i, bp = args
    return (i + 1) * search_a(bp, 24)


def lambda_b(bp):
    return search_a(bp, 32)


p = Pool()
puzzle.answer_a = sum(p.map(lambda_a, enumerate(blueprints)))
print(f"Puzzle answer a: {puzzle.answer_a}")
puzzle.answer_b = math.prod(p.map(lambda_b, blueprints[:3]))
print(f"Puzzle answer b: {puzzle.answer_b}")
