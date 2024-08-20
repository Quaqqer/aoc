from copy import deepcopy
from dataclasses import dataclass

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
