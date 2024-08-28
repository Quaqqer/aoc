import math
import re
from collections import defaultdict
from typing import cast

import graphlib
from aocd.models import Puzzle

puzzle = Puzzle(2019, int("14"))
data = puzzle.input_data
lines = data.splitlines()

recipes: dict[str, tuple[int, list[tuple[int, str]]]] = {}
graph = {}

for line in lines:
    ts = re.findall(r"(\d+) (\w+)", line)
    ts = [(int(n), cast(str, ty)) for n, ty in ts]
    *ingredients, result = ts
    res_n, res_t = result
    recipes[res_t] = (res_n, ingredients)
    graph[res_t] = [ing_ty for _, ing_ty in ingredients]


order = list(graphlib.TopologicalSorter(graph).static_order())[::-1]


def find_ore(fuel: int) -> int:
    neededs = defaultdict(int, {"FUEL": fuel})

    for em in order[:-1]:
        needed = neededs[em]

        got, ingredients = recipes[em]

        crafted = math.ceil(needed / got)

        for in_n, in_ty in ingredients:
            neededs[in_ty] += in_n * crafted

    return neededs["ORE"]


puzzle.answer_a = find_ore(1)


def binary_search() -> int:
    low = 0
    high = 10**12

    while low != high:
        m = (low + high) // 2

        cost = find_ore(m)

        if cost > 10**12:
            high = m - 1
        else:
            low = m

    return low


puzzle.answer_b = binary_search()
