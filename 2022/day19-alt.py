# 19   01:01:24    284      0   01:46:45    622      0
# bruteforce chads, we can't stop winning!

import functools
import math
import re

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("19"))
id = puzzle.input_data
id = """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."""


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


def dfs(blueprint: tuple[tuple[int, ...], ...], start_time: int) -> int:
    max_costs = tuple(map(max, lib.unzip(blueprint)))

    @functools.cache
    def inner(robots: tuple[int, ...], resources: tuple[int, ...], time: int) -> int:
        if time == 0:
            return robots[3]

        best = inner(robots, lib.tup_add(resources, robots[:3]), time - 1)

        for construct, cost in enumerate(blueprint):
            after_build = lib.tup_sub(resources, cost)

            if all(r >= 0 for r in after_build) and (
                construct == 3 or robots[construct] < max_costs[construct]
            ):
                next_robots = lib.tup_add(
                    robots, (0,) * construct + (1,) + (0,) * (3 - construct)
                )

                next_resources = tuple(
                    min(v, max_costs[i])
                    for i, v in enumerate(lib.tup_add(after_build, robots[:3]))
                )

                best = max(
                    best,
                    inner(next_robots, next_resources, time - 1),
                )

        return best + robots[3]

    return inner((1, 0, 0, 0), (0, 0, 0), start_time)


print(dfs(blueprints[0], 24))
# print(math.prod(dfs(blueprints[i], 32) for i in range(3)))
