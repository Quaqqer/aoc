import re
from collections import defaultdict

from aocd.models import Puzzle

puzzle = Puzzle(2017, int("08"))

data = puzzle.input_data

lines = data.splitlines()


registers = defaultdict(int)

max_ever = 0

for line in lines:
    print(line)
    match = re.match(r"(.*) (inc|dec) (.*) if (.*) (>|<|>=|==|<=|!=) (-?\d+)", line)
    assert match is not None
    target, op, x, cond_reg, cond_op, cond_v = match.groups()
    if eval(f"{registers[cond_reg]} {cond_op} {cond_v}"):
        x = int(x)
        delta = x * (1 if op == "inc" else -1)
        registers[target] += delta
        max_ever = max(max_ever, registers[target])

puzzle.answer_a = max(registers.values())
puzzle.answer_b = max_ever
