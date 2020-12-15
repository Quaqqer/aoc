#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=15);
input = [int(i) for i in puzzle.input_data.split(",")]

before = {}
n = 0
last = None
for i in input:
    if last != None:
        before[last] = n
    last = i

    n += 1

while (n < 2020):
    next = 0

    if last in before:
        next = n - before[last]

    before[last] = n
    last = next
    n += 1

silver = last
print(f"Silver: {silver}")

while (n < 30000000):
    next = 0

    if last in before:
        next = n - before[last]

    before[last] = n
    last = next
    n += 1

gold = last
print(f"Gold: {gold}")
