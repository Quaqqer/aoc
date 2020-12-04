#!/bin/python3
from aocd.models import Puzzle

puzzle = Puzzle(2020, 1)
lines = puzzle.input_data.split('\n')

vals = [int(line) for line in lines]

for i in range(len(vals)):
    for j in range(i + 1, len(vals)):
        if (vals[i] + vals[j] == 2020):
            print(f"Answer a: {vals[i] * vals[j]}")

for i in range(len(vals)):
    for j in range(i + 1, len(vals)):
        for k in range(j + 1, len(vals)):
            if (vals[i] + vals[j] + vals[k] == 2020):
                print(f"Answer b: {vals[i] * vals[j] * vals[k]}")
