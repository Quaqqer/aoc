# 13   00:40:42   2810      0   00:51:38   2694      0
# Pretty slow today, got stuck on equal comparisons between lists

import functools
import math

import numpy as np
from aocd.models import Puzzle

puzzle = Puzzle(2022, int("13"))
id = puzzle.input_data


# Main code
to_compare = [
    tuple(eval(line) for line in chunk.splitlines()) for chunk in id.split("\n\n")
]


def compare(left, right) -> int:
    match left, right:
        case int(l), int(r):
            return np.sign(r - l)
        case int(l), list(r):
            return compare([l], r)
        case list(l), int(r):
            return compare(l, [r])
        case [], []:
            return 0
        case [], [rhead, *rtail]:
            return 1
        case [lhead, *ltail], []:
            return -1
        case [lhead, *ltail], [rhead, *rtail]:
            head_comparison = compare(lhead, rhead)
            return compare(ltail, rtail) if head_comparison == 0 else head_comparison
        case _, _:
            raise Exception("Unreachable")


puzzle.answer_a = sum(
    i + 1 for i, (l, r) in enumerate(to_compare) if compare(l, r) == 1
)


sorted_packets = sorted(
    [eval(line) for line in id.splitlines() if line] + [[[2]], [[6]]],
    key=functools.cmp_to_key(lambda l, r: compare(r, l)),
)

puzzle.answer_b = math.prod(sorted_packets.index(p) + 1 for p in [[[2]], [[6]]])
