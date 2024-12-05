# 5   00:43:48  8852      0   00:57:11  6629      0
#
# Embarrassing... I really thought rules were transitive. If you have a rule
# that 1 comes before 2 and 2 comes before 3 it's reasonable to expect that 3
# comes after 1. It worked in the example input also... :'(

from collections import defaultdict
from typing import Sequence

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2024, int("05"))

data = puzzle.input_data

ruless, tests = [[ints(l) for l in ls.splitlines()] for ls in data.split("\n\n")]


r = defaultdict(list)
ri = defaultdict(list)
for rule in ruless:
    a, b = rule
    r[b].append(a)
    ri[a].append(b)


def correct_order(s: Sequence[int]):
    for i in range(len(s)):
        for j in range(i + 1, len(s)):
            a, b = s[i], s[j]
            if b in r[a]:
                return False
    return True


def order_correctly(s: Sequence[int]) -> list[int]:
    n = []

    def insert(i):
        v = s[i]
        if v in n:
            return
        for c in ri[v]:
            if c in s:
                insert(s.index(c))
        n.append(v)

    for i in range(len(s)):
        insert(i)

    return n


a = sum(s[len(s) // 2] for s in tests if correct_order(s))
puzzle.answer_a = a
puzzle.answer_b = (
    sum(oc[len(oc) // 2] for oc in (order_correctly(s) for s in tests)) - a
)
