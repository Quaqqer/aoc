import itertools
import re
from collections import defaultdict

from aocd.models import Puzzle

puzzle = Puzzle(2015, int("13"))
data = puzzle.input_data
lines = data.splitlines()

happiness: dict[str, dict[str, int]] = defaultdict(dict)

for line in lines:
    m = re.match(
        r"(.*) would ([^ ]*) (.*) happiness units by sitting next to (.*)\.", line
    )
    assert m is not None
    a, w, h, b = m.groups()
    happiness[a][b] = int(h) if w == "gain" else -int(h)


def score_happiness(l: tuple[str, ...]) -> int:
    tot = 0
    for a, b in zip(l, l[1:] + l[:1]):
        tot += happiness[a][b]
        tot += happiness[b][a]
    return tot


puzzle.answer_a = max(
    score_happiness(p) for p in itertools.permutations(happiness.keys())
)

happiness["me"] = {p: 0 for p in happiness.keys()}
for p in happiness.keys():
    happiness[p]["me"] = 0

puzzle.answer_b = max(
    score_happiness(p) for p in itertools.permutations(happiness.keys())
)
