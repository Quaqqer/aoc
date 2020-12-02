#!/bin/python3
from aocd.models import Puzzle
import re

puzzle = Puzzle(year=2020, day=2)
input = puzzle.input_data.split('\n')

rex = re.compile("([0-9]+)-([0-9]+) (.): (.*)")

def valid1(line):
    match = rex.match(line)
    min = int(match.group(1))
    max = int(match.group(2))
    char = match.group(3)
    password = match.group(4)
    count = 0
    for c in password:
        if c == char:
            count += 1
            if count > max:
                return False
    return count >= min


sum = 0
for line in input:
    if (valid1(line)):
        sum += 1

print(f"Answer a: {sum}")

def valid2(line):
    match = rex.match(line)
    p1 = int(match.group(1))
    p2 = int(match.group(2))
    char = match.group(3)
    password = match.group(4)
    first = password[p1 - 1] == char
    second = password[p2 - 1] == char
    return not (first and second) and (first or second)


sum = 0
for line in input:
    if (valid2(line)):
        sum += 1

print(f"Answer b: {sum}")
