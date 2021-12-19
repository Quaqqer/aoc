#!/usr/bin/env python3
import re
from collections import defaultdict

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 14)
seq, rules = tuple(puzzle.input_data.split("\n\n"))
rules = rules.splitlines()


# Main code
ruleset = {}

reg = re.compile("([A-Z][A-Z]) -> ([A-Z])")
for rule in rules:
    m = reg.fullmatch(rule)
    assert m
    key = m.group(1)
    new = m.group(2)

    ruleset[key] = new


def step(poly):
    new_poly = defaultdict(int)
    for k, v in poly.items():
        new_poly[k[0] + ruleset[k]] += v
        new_poly[ruleset[k] + k[1]] += v
    return new_poly


def score(poly, last):
    counts = defaultdict(int)
    for k, v in poly.items():
        counts[k[0]] += v
    counts[last] += 1
    return max(counts.values()) - min(counts.values())


initial = defaultdict(int)
for i in range(len(seq) - 1):
    initial[seq[i:i + 2]] += 1

poly_silver = initial
for i in range(10):
    poly_silver = step(poly_silver)
silver = score(poly_silver, seq[-1])

poly_gold = initial
for i in range(40):
    poly_gold = step(poly_gold)
gold = score(poly_gold, seq[-1])


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
