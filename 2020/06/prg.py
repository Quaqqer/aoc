#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=6);
input = puzzle.input_data
groups = [group.split("\n") for group in input.split("\n\n")]

ans_a = 0
for group in groups:
    yes = set()
    for individual in group:
        for chr in individual:
            yes.add(chr)
    ans_a += len(yes)
print(f"Answer A: {ans_a}")

ans_b = 0
for group in groups:
    yes = {}
    for individual in group:
        for chr in individual:
            if chr.isalpha():
                if chr in yes:
                    yes[chr] += 1
                else:
                    yes[chr] = 1
    for k, v in yes.items():
        if v == len(group):
            ans_b += 1
print(f"Answer B: {ans_b}")
