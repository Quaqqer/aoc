#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=22);
data = puzzle.input_data
input = [i.split("\n") for i in data.split("\n\n")]

p1 = [int(i) for i in input[0][1:]]
p2 = [int(i) for i in input[1][1:]]

while (p1 and p2):
    v1 = p1.pop(0)
    v2 = p2.pop(0)
    if v1 > v2:
        p1.append(v1)
        p1.append(v2)
    else:
        p2.append(v2)
        p2.append(v1)

winner = list(reversed(p1 if p1 else p2))
silver = 0
for i, v in enumerate(winner):
    silver += (i + 1) * v
print(silver)

def rec(p1, p2):
    prevboards = set()
    while(p1 and p2):
        if (tuple(p1), tuple(p2)) in prevboards: return "p1", -1
        prevboards.add((tuple(p1), tuple(p2)))
        v1 = p1.pop(0)
        v2 = p2.pop(0)
        if v1 <= len(p1) and v2 <= len(p2):
            if rec(p1[:v1], p2[:v2])[0] == "p1":
                p1.append(v1)
                p1.append(v2)
            else:
                p2.append(v2)
                p2.append(v1)
        else:
            if v1 > v2:
                p1.append(v1)
                p1.append(v2)
            else:
                p2.append(v2)
                p2.append(v1)

    score = sum([(i + 1) * v for i, v in enumerate(list(reversed(p1 if p1 else p2)))])
    return "p1" if p1 else "p2", score

p1 = [int(i) for i in input[0][1:]]
p2 = [int(i) for i in input[1][1:]]
gold = rec(p1, p2)[1]
print(gold)
puzzle.answer_b = gold
