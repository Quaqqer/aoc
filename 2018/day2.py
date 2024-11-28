# Solved both parts in 3:16:11

from collections import Counter

from aocd.models import Puzzle

puzzle = Puzzle(2018, int("2"))
data = puzzle.input_data
lines = data.splitlines()

doubles = 0
triples = 0
for line in lines:
    if 2 in Counter(line).values():
        doubles += 1
    if 3 in Counter(line).values():
        triples += 1

puzzle.answer_a = doubles * triples

for a in lines:
    for b in lines:
        sames = ""
        diffs = 0
        for aa, bb in zip(a, b):
            if aa == bb:
                sames += aa
            if aa != bb:
                diffs += 1

                if diffs > 1:
                    break
        if diffs == 1:
            puzzle.answer_b = sames
