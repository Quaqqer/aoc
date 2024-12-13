# 5   00:43:48  8852      0   00:57:11  6629      0
#
# Embarrassing... I really thought rules were transitive. If you have a rule
# that 1 comes before 2 and 2 comes before 3 it's reasonable to expect that 3
# comes after 1. It worked in the example input also... :'(

import functools
from collections import defaultdict
from typing import Sequence

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2024, int("05"))

data = puzzle.input_data

ruless, tests = [[ints(l) for l in ls.splitlines()] for ls in data.split("\n\n")]


r = defaultdict(list)
for rule in ruless:
    a, b = rule
    r[b].append(a)


def correct_order(s: Sequence[int]):
    return not any(s[j] in r[s[i]] for i in range(len(s)) for j in range(i + 1, len(s)))


def order_correctly(s: Sequence[int]) -> list[int]:
    # Thanks for the idea Axel
    return sorted(s, key=functools.cmp_to_key(lambda a, b: -1 if a in r[b] else 0))


a = sum(s[len(s) // 2] for s in tests if correct_order(s))
b = sum(oc[len(oc) // 2] for oc in (order_correctly(s) for s in tests)) - a

puzzle.answer_a = a
puzzle.answer_b = b
