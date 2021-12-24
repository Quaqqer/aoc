#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 24)
lines = puzzle.input_data.split("\n")
line_chunks = []
while len(lines) > 0:
    line_chunks.append(lines[:18])
    lines = lines[18:]


# Observe: it gets divided by 26 if we mage sure x is false, what we want for it to
# reach 0.
def gentest(condadd, const):
    def test(digit, oldz):
        x = (oldz % 26) + condadd != digit
        z = (oldz // 26) * (26 if x else 1) + (digit + const) * int(x)
        return z

    return test


# these are the rules that apply for my input:
# observe that it follows a stack-like behaviour.
theories = [
    lambda digit, oldz: oldz * 26 + digit + 14,
    lambda digit, oldz: oldz * 26 + digit + 6,
    lambda digit, oldz: oldz * 26 + digit + 6,
    lambda digit, oldz: oldz * 26 + digit + 13,
    gentest(-12, 8),
    lambda digit, oldz: oldz * 26 + digit + 8,
    gentest(-15, 7),
    lambda digit, oldz: oldz * 26 + digit + 10,
    lambda digit, oldz: oldz * 26 + digit + 8,
    gentest(-13, 12),
    gentest(-13, 10),
    gentest(-14, 8),
    gentest(-2, 8),
    gentest(-9, 7),
]

# generate rules
maxs = [-1] * 14
mins = [-1] * 14
stack = []
for i, chunk in enumerate(line_chunks):
    if "div z 1" in "".join(chunk):
        value = int(chunk[15].split(" ")[2])
        item = (i, value)
        stack.append(item)
    else:
        oldi, value = stack.pop()
        cond = int(chunk[5].split(" ")[2])

        _v2 = value + cond
        for v1 in reversed(range(1, 9 + 1)):
            v2 = _v2 + v1
            if 1 <= v1 <= 9 and 1 <= v2 <= 9:
                maxs[oldi] = v1
                maxs[i] = v2
                break
        for v1 in range(1, 9 + 1):
            v2 = _v2 + v1
            if 1 <= v1 <= 9 and 1 <= v2 <= 9:
                mins[oldi] = v1
                mins[i] = v2
                break

silver = "".join(str(i) for i in maxs)
gold = "".join(str(i) for i in mins)

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
