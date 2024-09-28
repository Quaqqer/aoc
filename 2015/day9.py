import re
from collections import defaultdict
from typing import Iterable

from aocd.models import Puzzle

puzzle = Puzzle(2015, int("9"))
data = puzzle.input_data
lines = data.splitlines()

dists: dict[str, dict[str, int]] = defaultdict(dict)
locations = set()

for line in lines:
    m = re.match("(.*) to (.*) = (.*)", line)
    assert m
    [from_, to, cost] = m.groups()
    dists[from_][to] = int(cost)
    dists[to][from_] = int(cost)
    locations |= {from_, to}


def costs() -> Iterable[int]:
    q = [(l, {l}, 0) for l in locations]

    while q:
        location, visited, cost = q.pop()

        if visited == locations:
            yield cost

        for neigh, neigh_cost in dists[location].items():
            if neigh in visited:
                continue

            q.append((neigh, visited | {neigh}, cost + neigh_cost))


puzzle.answer_a = min(costs())
puzzle.answer_b = max(costs())
