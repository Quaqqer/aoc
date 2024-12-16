# 8   00:08:56  1828      0   00:51:21  3937      0
# why did they repeat perfectly?!?! sigh...

import math
from collections import defaultdict

from aocd.models import Puzzle

puzzle = Puzzle(2023, int("08"))
data = puzzle.input_data
lines = data.splitlines()

g = defaultdict(list)
lrs = lines[0]

for line in lines[2:]:
    from_, to = line.split(" = ")
    l, r = to[1:-1].split(", ")
    g[from_] = [l, r]


def steps(a: str, p2: bool) -> int:
    s = 0

    while (not p2 and a != "ZZZ") or (p2 and a[-1] != "Z"):
        l, r = g[a]
        if lrs[s % len(lrs)] == "L":
            a = l
        else:
            a = r
        s += 1

    return s


puzzle.answer_a = steps("AAA", False)
puzzle.answer_b = math.lcm(*(steps(a, True) for a in g if a[-1] == "A"))
