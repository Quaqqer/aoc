#!/bin/python3
import re
from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=4);
input = puzzle.input_data.split("\n\n")

def validate_a(fields):
    for field in fields:
        if field not in {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"}:
            return False
    return (len(fields) == 7 and "cid" not in fields) or len(fields) == 8

def hgt(x):
    match = re.fullmatch("([0-9]+)(cm|in)", x)
    if not match:
        return False
    if match.group(2) == "cm":
        return 150 <= int(match.group(1)) <= 193
    else: # group(2) == "in"
        return 59 <= int(match.group(1)) <= 76

validation_map = {
    "byr": lambda x: re.fullmatch("[0-9]{4}", x) and 1920 <= int(x) <= 2002,
    "iyr": lambda x: re.fullmatch("[0-9]{4}", x) and 2010 <= int(x) <= 2020,
    "eyr": lambda x: re.fullmatch("[0-9]{4}", x) and 2020 <= int(x) <= 2030,
    "hgt": hgt,
    "hcl": lambda x: bool(re.fullmatch("#[0-9a-f]{6}", x)),
    "ecl": lambda x: x in {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"},
    "pid": lambda x: bool(re.fullmatch("[0-9]{9}", x)),
    "cid": lambda x: True,
}

def validate_b(fields):
    for field, value in fields.items():
        if field not in validation_map or not validation_map[field](value):
            return False
    return (len(fields) == 7 and "cid" not in fields) or len(fields) == 8

valid_a = 0
valid_b = 0

fieldx = re.compile("([a-z]+):([#a-z0-9]+)")
for i in input:
    st = {match[0]: match[1] for match in fieldx.findall(i)}

    if validate_a(st): valid_a += 1
    if validate_b(st): valid_b += 1

print(f"Answer A: {valid_a}")
print(f"Answer B: {valid_b}")
