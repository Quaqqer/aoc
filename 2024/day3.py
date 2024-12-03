# 3   00:07:40  2920      0   00:08:53   840      0
#
# I really need to stop using puzzle.answer_a/b to submit my answers, I keep on
# submitting wrong answers. Better to just print them...
#
# I also accidentally had all my code inside of a for line in lines loop...
# Time to remove that from my template...

import re

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2024, int("03"))
data = puzzle.input_data
lines = data.splitlines()

instructions_a = re.findall(r"mul\(\d+,\d+\)", data)
instructions_b = re.findall(r"mul\(\d+,\d+\)|do\(\)|don't\(\)", data)


def compute(instructions: list[str]):
    m = 0
    do = True
    for mul in instructions:
        if "mul" in mul and do:
            a, b = ints(mul)
            m += a * b
        elif "don't" in mul:
            do = False
        elif "do" in mul:
            do = True
    return m


puzzle.answer_a = compute(instructions_a)
puzzle.answer_b = compute(instructions_b)
