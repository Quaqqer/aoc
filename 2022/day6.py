# 6   00:04:48  1853      0   00:06:21  2048      0
# 4:48 is also slow I guess

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("06"))
id = puzzle.input_data


def marker(n: int) -> int:
    for i in range(len(id)):
        if len(set(id[i : i + n])) == n:
            return i + n
    raise Exception("Should not fail")


puzzle.answer_a = marker(4)
puzzle.answer_b = marker(14)
