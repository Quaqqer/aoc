#!/bin/python3
import re
from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=4);
input = puzzle.input_data.split("\n\n")

requirements = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

def validate_a(fields):
    searchedfields = set()
    for field in fields:
        if field == "cid":
            continue
        searchedfields.add(field)
        if field not in requirements:
            return False
    return len(searchedfields) == 7

def hgt(x):
    match = re.match("([0-9]+)(cm|in)", x)
    if not match:
        return False
    if match.group(2) == "cm":
        return 150 <= int(match.group(1)) <= 193
    if match.group(2) == "in":
        return 59 <= int(match.group(1)) <= 76

def hcl(x):
    match = re.match("#[0-9a-f]*", x)
    if not match:
        return False
    return len(match.group(0)) == 7

validation_map = {
    "byr": lambda x: 1920 <= int(x) <= 2002,
    "iyr": lambda x: 2010 <= int(x) <= 2020,
    "eyr": lambda x: 2020 <= int(x) <= 2030,
    "hgt": hgt,
    "hcl": hcl,
    "ecl": lambda x: x in {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"},
    "pid": lambda x: len(re.match("[0-9]*", x).group(0)) == 9
}

def validate_b(fields):
    searchedfields = set()
    for field, value in fields.items():
        if field == "cid":
            continue
        if not field in requirements:
            return False
        searchedfields.add(field)
        if not validation_map[field](value):
            return False
    return len(searchedfields) == 7

valid_a = 0
valid_b = 0

fieldx = re.compile("([a-z]+):([#a-z0-9]+)")
for i in input:
    matches = fieldx.findall(i)
    st = {match[0]: match[1] for match in matches}

    if validate_a(st): valid_a += 1
    if validate_b(st): valid_b += 1

print(f"Answer A: {valid_a}")
print(f"Answer B: {valid_b}")
