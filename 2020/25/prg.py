#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=25);
input = [int(i) for i in puzzle.input_data.split("\n")]
#input = [5764801, 17807724]

loop1 = 0
v1 = 1
while not v1 == input[0]:
    v1 *= 7
    v1 = v1 % 20201227
    loop1 += 1

v2 = input[1]
v3 = 1
for i in range(loop1):
    v3 *= v2
    v3 = v3 % 20201227
print(f"Silver: {v3}")
