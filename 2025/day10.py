# 10   01:55:31   02:11:15
# Woke up 7:05, started 7:06 (so 1 hour after first start)

# pyright: basic


import functools

import z3
from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2025, int("10"))


def parse_a(line: str):
    machine, *presses, _costs = line.split(" ")

    machine = machine[1:-1]

    ans = 0

    start = 0
    for i, c in enumerate(machine):
        start += (c == "#") << i

    press_vs = []
    for i, press in enumerate(presses):
        press = ints(press)
        press_v = 0
        for p in press:
            press_v |= 1 << p
        press_vs.append(press_v)

    return start, ans, press_vs


def minimize(line: str) -> int:
    start, ans, press_vs = parse_a(line)

    @functools.cache
    def press(v: int, i: int) -> int | None:
        if i == len(press_vs):
            if v == ans:
                return 0
            else:
                return None

        l_cost = press(v ^ press_vs[i], i + 1)
        if l_cost is not None:
            l_cost += 1

        r_cost = press(v, i + 1)

        if l_cost is not None:
            if r_cost is not None:
                return min(l_cost, r_cost)
            return l_cost
        return r_cost

    answer = press(start, 0)
    assert answer is not None
    return answer


def solve_b(line: str) -> int:
    _, *presses, costs = line.split(" ")
    presses = [ints(p) for p in presses]
    costs = ints(costs)

    o = z3.Optimize()

    cs = [z3.Int(f"c{i}") for i in range(len(costs))]
    for c in cs:
        o.add(c == 0)

    ns = []
    for i, press in enumerate(presses):
        n = z3.Int(f"n{i}")
        o.add(n >= 0)
        ns.append(n)

        for i in press:
            cs[i] += n

    for i, cost in enumerate(costs):
        o.add(cs[i] == cost)

    sum_v = z3.Int("sum")
    o.add(sum_v == z3.Sum(ns))
    o.minimize(sum_v)
    o.check()
    m = o.model()
    x = m[sum_v]
    assert isinstance(x, z3.IntNumRef)
    return x.as_long()


puzzle.answer_a = sum(minimize(line) for line in puzzle.input_data.splitlines())
puzzle.answer_b = sum(solve_b(line) for line in puzzle.input_data.splitlines())
