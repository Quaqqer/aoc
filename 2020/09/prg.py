#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=9);
input = [int(i) for i in puzzle.input_data.split("\n")]

pa = 25 # preamble size
prev = [] # list 

for i in range(pa):
    prev.append(input[i])

def is_sum(i):
    for x in range(len(prev)):
        for y in range(x + 1, len(prev)):
            if prev[x] + prev[y] == i:
                return True
    return False

silver = 0
for i in range(pa, len(input)):
    if not is_sum(input[i]):
        silver = input[i]
        print(f"Silver: {silver}")
    prev.pop(0)
    prev.append(input[i])

candidate = None
for i in range(len(input)):
    v = input[i]
    curr = [input[i]]
    for j in range(i + 1, len(input)):
        if sum(curr) + input[j] > silver:
            break
        else:
            curr.append(input[j])
    if sum(curr) == silver:
        candidate = curr
        break
candidate.sort()
gold = candidate[0] + candidate[-1]
print(f"Gold: {gold}")
puzzle.answer_b = gold
