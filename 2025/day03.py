# 3   00:01:50   00:17:41
# I can't be stopped with good part 1 times?! Tried to solve it too quickly in
# part 2 and got a 5 minute timeout. I should really assert that the answer for
# the example is correct. 😅


# pyright: basic

import functools

from aocd.models import Puzzle

puzzle = Puzzle(2025, int("03"))


def max_bat_greedy(s: str, digits: int) -> int:
    current = ""

    i = 0
    while digits > 0:
        j = max(range(i, len(s) - digits + 1), key=lambda j: s[j])
        current += s[j]
        i = j + 1
        digits -= 1

    return int(current)


def max_bat_dp(s: str, digits: int) -> int:
    @functools.cache
    def inner(start: int, digits: int) -> int | None:
        best_jolts = None

        for i in range(start, len(s)):
            rest = inner(i + 1, digits - 1) if digits > 1 else ""

            if rest is None:
                continue

            jolts = int(s[i] + str(rest))

            if best_jolts is None or best_jolts < jolts:
                best_jolts = jolts

        return best_jolts

    jolts = inner(0, digits)
    assert jolts is not None
    return jolts


def solve(input: str, digits: int):
    return sum(max_bat_greedy(line, digits) for line in input.splitlines())


puzzle.answer_a = solve(puzzle.input_data, 2)
puzzle.answer_b = solve(puzzle.input_data, 12)
