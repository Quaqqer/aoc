from aocd.models import Puzzle

import util

puzzle = Puzzle(2019, int("03"))
data = puzzle.input_data
lines = data.splitlines()


def draw_line(instructions: str) -> dict[tuple[int, int], int]:
    walked = {}

    x, y = 0, 0
    steps = 0
    for instruction in instructions.split(","):
        dx, dy = {"R": (1, 0), "L": (-1, 0), "U": (0, 1), "D": (0, -1)}[instruction[0]]
        for _ in range(int(instruction[1:])):
            x, y = x + dx, y + dy
            steps += 1
            walked[x, y] = steps

    return walked


a = draw_line(lines[0])
b = draw_line(lines[1])

puzzle.answer_a = min(util.manhattan(p) for p in a if p in b)
puzzle.answer_b = min(a[p] + b[p] for p in a if p in b)
