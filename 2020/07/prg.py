#!/bin/python3

import re
from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=7);
input = puzzle.input_data.split("\n")

bax = re.compile("([a-z]+ [a-z]+) bags contain")
res = re.compile("([0-9]+) ([a-z]+ [a-z]+) bag")

bagmap = {}
for line in input:
    key = bax.match(line).group(1)
    values = [(int(result[0]), result[1]) for result in res.findall(line)]
    bagmap[key] = values

def contains_goldbag(bag):
    for amt, color in bagmap[bag]:
        if color == "shiny gold" or contains_goldbag(color):
            return True
    return False

silver = 0
for bag in bagmap:
    if bag != "shiny gold" and contains_goldbag(bag):
        silver += 1
print(f"Silver: {silver}")

def content(bag):
    sum = 0
    for amt, color in bagmap[bag]:
        sum += amt * (content(color) + 1)
    return sum

gold = content("shiny gold")
print(f"Gold: {gold}")
