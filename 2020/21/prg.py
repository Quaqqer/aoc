#!/bin/python3

import re
from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=21);
data = puzzle.input_data
input = data.split("\n")

ingredients = set()
allergens = {}

for line in input:
    ings = re.findall("[a-zA-Z]+", line.split("(")[0])
    alls = re.findall("[a-zA-Z]+", line.split("contains")[1])
    ingredients = ingredients.union(set(ings))
    for all in alls:
        if not all in allergens:
            allergens[all] = set(ings)
        else:
            allergens[all] = allergens[all].intersection(set(ings))

change = True
while (change):
    change = False
    for i in allergens:
        for j in allergens:
            if len(allergens[i]) == 1 and len(allergens[j]) != 1:
                oldsize = len(allergens[j])
                allergens[j] = allergens[j] - allergens[i]
                if len(allergens[j]) != oldsize:
                    change = True

non_allergens = ingredients
for all, ings in allergens.items():
    non_allergens -= ings

silver = 0
for line in input:
    ings = re.findall("[a-zA-Z]+", line.split("(")[0])
    for ing in ings:
        if ing in non_allergens: silver += 1
print(f"Silver: {silver}")

alphall = [(all, ing.pop()) for all, ing in allergens.items()]
alphall.sort()
gold = ",".join([alphall[i][1] for i in range(len(alphall))])
print(f"Gold: {gold}")
