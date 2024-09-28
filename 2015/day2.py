from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2015, int("02"))
data = puzzle.input_data
lines = data.splitlines()


def paper(l: int, w: int, h: int) -> int:
    surfaces = [l * w, w * h, h * l] * 2
    return sum(surfaces) + min(surfaces)


def ribbon(l: int, w: int, h: int) -> int:
    perimeters = [2 * (l + w), 2 * (w + h), 2 * (h + l)]
    return min(perimeters) + l * w * h


dimensions = [ints(line) for line in lines]

puzzle.answer_a = sum(paper(*d) for d in dimensions)
puzzle.answer_b = sum(ribbon(*d) for d in dimensions)
