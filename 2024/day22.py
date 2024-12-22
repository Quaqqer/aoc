# 22   00:07:20   821      0   00:57:41  2121      0
# My first solution used multiprocessing and it took 1 minute 😬
# This is my second solution, it's a lot faster!

from collections import defaultdict

from aocd.models import Puzzle

puzzle = Puzzle(2024, int("22"))
data = puzzle.input_data

lines = data.splitlines()


def next_secret(secret: int):
    secret ^= secret * 64
    secret %= 16777216
    secret ^= secret // 32
    secret %= 16777216
    secret ^= secret * 2048
    secret %= 16777216
    return secret


def solve_a():
    s = 0
    for line in lines:
        secret = int(line)
        for _ in range(2000):
            secret = next_secret(secret)
        s += secret
    return s


def solve_b():
    deltas = defaultdict(int)

    for line in lines:
        seq = [int(line)]
        for _ in range(2000):
            seq.append(next_secret(seq[-1]))

        ps = [v % 10 for v in seq]
        changes = tuple(b - a for a, b in zip(ps, ps[1:]))
        delta = {}

        for i in range(len(changes) - 3)[::-1]:
            d = changes[i : i + 4]
            p = ps[i + 4]
            delta[d] = p

        for d, p in delta.items():
            deltas[d] += p

    return max(deltas.values())


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
