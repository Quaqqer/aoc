from aocd.models import Puzzle
from util import Vec2

puzzle = Puzzle(2015, int("03"))
data = puzzle.input_data
lines = data.splitlines()


dirs = {
    "^": Vec2(0, -1),
    "<": Vec2(-1, 0),
    ">": Vec2(1, 0),
    "v": Vec2(0, 1),
}


def solve_a():
    pos = Vec2(0, 0)

    presents: set[Vec2] = {pos}

    for c in data:
        pos += dirs[c]
        presents |= {pos}

    return len(presents)


def solve_b():
    santa = Vec2(0, 0)
    robo = Vec2(0, 0)
    presents: set[Vec2] = {santa}

    for i, c in enumerate(data):
        if i % 2 == 0:
            santa += dirs[c]
            presents |= {santa}
        else:
            robo += dirs[c]
            presents |= {robo}

    return len(presents)


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
