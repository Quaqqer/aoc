from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2015, int("6"))
data = puzzle.input_data
lines = data.splitlines()


def solve_a():
    lights = [False] * 1000 * 1000
    for line in lines:
        ax, ay, bx, by = ints(line)

        match line.split():
            case "turn", "on", _, "through", _:
                for x in range(ax, bx + 1):
                    for y in range(ay, by + 1):
                        lights[y * 1000 + x] = True

            case "turn", "off", _, "through", _:
                for x in range(ax, bx + 1):
                    for y in range(ay, by + 1):
                        lights[y * 1000 + x] = False
            case "toggle", _, "through", _:
                for x in range(ax, bx + 1):
                    for y in range(ay, by + 1):
                        lights[y * 1000 + x] ^= True

            case _:
                raise Exception()

    return sum(lights)


def solve_b():
    brightness = [0] * 1000 * 1000
    for line in lines:
        ax, ay, bx, by = ints(line)

        match line.split():
            case "turn", "on", _, "through", _:
                for x in range(ax, bx + 1):
                    for y in range(ay, by + 1):
                        brightness[y * 1000 + x] += 1

            case "turn", "off", _, "through", _:
                for x in range(ax, bx + 1):
                    for y in range(ay, by + 1):
                        brightness[y * 1000 + x] = max(0, brightness[y * 1000 + x] - 1)
            case "toggle", _, "through", _:
                for x in range(ax, bx + 1):
                    for y in range(ay, by + 1):
                        brightness[y * 1000 + x] += 2

            case _:
                raise Exception()

    return sum(brightness)


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
