import math
from copy import deepcopy
from dataclasses import dataclass
from typing import Literal, cast

from aocd.models import Puzzle

from util import Vec3, ints, spaceship

puzzle = Puzzle(2019, int("12"))
data = puzzle.input_data
lines = data.splitlines()


@dataclass
class Moon:
    pos: Vec3[int]
    vel: Vec3[int]


moons = [Moon(pos=Vec3(x, y, z), vel=Vec3(0, 0, 0)) for x, y, z in map(ints, lines)]


def step(moons: list[Moon]):
    for moon in moons:
        for other in moons:
            if moon is other:
                continue

            moon.vel += Vec3(
                -spaceship(moon.pos.x, other.pos.x),
                -spaceship(moon.pos.y, other.pos.y),
                -spaceship(moon.pos.z, other.pos.z),
            )

    for moon in moons:
        moon.pos += moon.vel


moons_a = deepcopy(moons)

for _ in range(1000):
    step(moons_a)

puzzle.answer_a = sum(
    sum(abs(moon.pos).to_tuple()) * sum(abs(moon.vel).to_tuple()) for moon in moons_a
)

# For b, simulate it all but keep track of seen states in each respective
# dimension, since dimensions are independent.
moons_b = deepcopy(moons)

type Dim = Literal["x"] | Literal["y"] | Literal["z"]

dims: tuple[Dim, ...] = "x", "y", "z"
seen: dict[Dim, set[tuple[tuple[int, int], ...]]] = {dim: set() for dim in dims}
cycles: dict[Dim, int | None] = {dim: None for dim in dims}

s = 0

while any(cycle is None for cycle in cycles.values()):
    for dim in dims:
        if cycles[dim] is not None:
            continue

        state = tuple(
            (getattr(moon.pos, dim), getattr(moon.vel, dim)) for moon in moons_b
        )

        if state in seen[dim]:
            cycles[dim] = s
        else:
            seen[dim].add(state)

    step(moons_b)
    s += 1

puzzle.answer_b = math.lcm(*(cast(int, cycle) for cycle in cycles))
