from aocd.models import Puzzle

puzzle = Puzzle(2015, int("10"))
data = puzzle.input_data


def step(s: str) -> str:
    buf = ""

    ch = s[0]
    n = 1
    i = 1
    while i < len(s):
        if s[i] == ch:
            n += 1
        else:
            buf += f"{n}{ch}"
            ch = s[i]
            n = 1

        i += 1
    buf += f"{n}{ch}"
    return buf


s = data
for _ in range(40):
    s = step(s)

puzzle.answer_a = len(s)

for _ in range(10):
    s = step(s)

puzzle.answer_b = len(s)
