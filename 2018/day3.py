# Part 1 solved in 1:57
# Part 2 solved in 3:20 (delta 1:23)

from collections import defaultdict

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2018, int("3"))
data = puzzle.input_data
lines = data.splitlines()

ids: set[int] = set()

n = defaultdict(set)
for line in lines:
    id, sx, sy, w, h = ints(line)

    ids |= {id}

    for dx in range(w):
        for dy in range(h):
            n[sx + dx, sy + dy] |= {id}

puzzle.answer_a = sum(2 <= len(v) for v in n.values())

[non_overlapping] = list(
    ids - set(id for ids in n.values() if len(ids) >= 2 for id in ids)
)
puzzle.answer_b = non_overlapping
