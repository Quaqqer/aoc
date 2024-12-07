# 7   00:12:06  1951      0   00:13:09  1173      0
# Once more I didn't read the instructions, I thought precedence was
# important...


from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2024, int("07"))

data = [ints(l) for l in puzzle.input_data.splitlines()]


def solves(ans: int, ns: list[int], b: bool):
    def inner(i: int, lhs: int):
        if i == len(ns):
            return lhs == ans

        return (
            inner(i + 1, lhs * ns[i])
            or inner(i + 1, lhs + ns[i])
            or (b and inner(i + 1, int(str(lhs) + str(ns[i]))))
        )

    return inner(1, ns[0])


puzzle.answer_a = sum(ans for [ans, *ns] in data if solves(ans, ns, False))
puzzle.answer_b = sum(ans for [ans, *ns] in data if solves(ans, ns, True))
