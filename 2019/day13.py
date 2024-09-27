from collections import Counter

import util
from aocd.models import Puzzle
from intpc import IntPC

puzzle = Puzzle(2019, int("13"))
data = puzzle.input_data
program = util.ints(data.split(","))


def render(output: list[int]) -> tuple[dict[tuple[int, int], str | int], int]:
    score = -1

    screen = {}
    for x, y, id in util.chunk(output, 3):
        if x == -1 and y == 0:
            score = id
            continue

        char = {0: " ", 1: "░", 2: "█", 3: "▔", 4: "●"}[id]
        screen[x, y] = char

    return screen, score


def solve_a() -> int:
    out = IntPC(program).io([])
    screen, _ = render(out)
    return Counter(screen.values())["█"]


def solve_b(show=False) -> int:
    intpc = IntPC(program)
    intpc.mem[0] = 2

    score = 0
    screen = {}
    while True:
        out = intpc.io([])

        new_screen, score = render(out)

        if intpc.is_halted:
            return score

        screen |= new_screen

        ks = list(screen.keys())
        vs = list(screen.values())

        ball_x = ks[vs.index("●")][0]
        paddle_x = ks[vs.index("▔")][0]

        intpc.queue_input([util.spaceship(ball_x, paddle_x)])

        if show:
            util.clear_term()
            util.print_set_grid(screen)


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
