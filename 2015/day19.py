import re

from aocd.models import Puzzle

puzzle = Puzzle(2015, int("19"))

data = puzzle.input_data

replacements, m = data.split("\n\n")
replacements = replacements.splitlines()
replacements = [(k, v) for k, v in (r.split(" => ") for r in replacements)]


def possibles(m: str) -> set[str]:
    s = set()

    for m1, m2 in replacements:
        for match in re.finditer(re.escape(m1), m):
            s |= {m[: match.start()] + m2 + m[match.end() :]}

    return s


puzzle.answer_a = len(possibles(m))
