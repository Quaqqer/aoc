# Part 1: 9:15
# Part 2: 22:25

from collections import defaultdict

from aocd.models import Puzzle

puzzle = Puzzle(2018, int("07"))
data = puzzle.input_data
lines = data.splitlines()

BASE_TIME = 60
WORKERS = 5


alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


def solve_a():
    deps: dict[str, list[str]] = defaultdict(list)

    for line in lines:
        _, a, _, _, _, _, _, b, _, _ = line.split()
        deps[b].append(a)

    s = ""

    while len(s) != len(deps):
        for c in alphabet:
            if deps[c] == [] and c not in s:
                s += c
                for dep in deps.values():
                    if c in dep:
                        dep.remove(c)
                break

    return s


def solve_b():
    deps: dict[str, list[str]] = defaultdict(list)

    for line in lines:
        _, a, _, _, _, _, _, b, _, _ = line.split()
        deps[b].append(a)
        deps[a]

    times = {c: BASE_TIME + ord(c) - ord("A") + 1 for c in deps}

    t = 0
    while True:
        w = WORKERS

        to_remove = []
        for c in deps:
            if deps[c] == [] and times[c] > 0 and w > 0:
                times[c] -= 1
                w -= 1

                if times[c] == 0:
                    to_remove.append(c)

        for c in to_remove:
            for dep in deps.values():
                if c in dep:
                    dep.remove(c)

        if w == WORKERS:
            return t

        t += 1


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
