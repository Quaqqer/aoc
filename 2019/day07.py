import itertools

import util
from aocd.models import Puzzle
from intpc import IntPC, State

puzzle = Puzzle(2019, int("07"))
data = puzzle.input_data
program = util.ints(data.split(","))


def run_a(phases: tuple[int, int, int, int, int]) -> int:
    v = 0
    for phase in phases:
        v = IntPC(program).io([phase, v])[-1]
    return v


def run_b(phases: tuple[int, int, int, int, int]) -> int:
    pcs = [IntPC(program, [phase]) for phase in phases]

    v = 0
    e = 0

    while True:
        for i, intpc in enumerate(pcs):
            if intpc.state == State.HALTED:
                return e

            v = intpc.io([v])[-1]

            if i == 4:
                e = v

            i = (i + 1) % 5


puzzle.answer_a = max(
    run_a(phases) for phases in itertools.permutations([0, 1, 2, 3, 4, 5], 5)
)

puzzle.answer_b = max(
    run_b(phases) for phases in itertools.permutations([5, 6, 7, 8, 9], 5)
)
