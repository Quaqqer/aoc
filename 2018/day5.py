# Rough times: 5:00 and 8:05 for part 1 and part 2 respectively

from aocd.models import Puzzle

puzzle = Puzzle(2018, int("05"))
data = puzzle.input_data

alphabet = "abcdefghijklmnopqrstuvwxyz"


def react(s):
    i = 0
    next_s = []
    while i < len(s) - 1:
        a, b = s[i], s[i + 1]
        if (
            a.lower() == b.lower()
            and any(c.isupper() for c in [a, b])
            and any(c.islower() for c in [a, b])
        ):
            i += 2
        else:
            i += 1
            next_s.append(s[i])
    return "".join(next_s)


def shortest(data):
    s = data
    while True:
        s1 = react(s)
        if s1 == s:
            return s
        else:
            s = s1


puzzle.answer_b = len(shortest(data))
puzzle.answer_b = min(
    len(shortest("".join(d for d in data if d.lower() != c))) for c in alphabet
)
