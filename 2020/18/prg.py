#!/bin/python3

import re
from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=18);
input = puzzle.input_data.split("\n")

num = re.compile("[0-9]+")

def cvrt(i: str):
    i = i.replace(" ", "")
    res = []
    while (i != ""):
        n = num.match(i)
        if n:
            i = i[len(n.group(0)):]
            res.append(int(n.group(0)))
        elif i.startswith("("):
            bal = 1
            pos = -1
            for j in range(1, len(i)):
                if i[j] == "(":
                    bal += 1
                elif i[j] == ")":
                    bal -= 1
                if bal == 0:
                    pos = j
                    break
            res.append(cvrt(i[1:pos]))
            i = i[pos+1:]
        elif i[0] == "*":
            res.append("*")
            i = i[1:]
        elif i[0] == "+":
            res.append("+")
            i = i[1:]
        else:
            print("WHAT")
            print(i)
    return res

def evaluate_a(lst):
    if isinstance(lst, int):
        return lst
    elif len(lst) == 1:
        return evaluate_a(lst[0])
    elif len(lst) == 3:
        if lst[1] == "+":
            return evaluate_a(lst[0]) + evaluate_a(lst[2])
        elif lst[1] == "*":
            return evaluate_a(lst[0]) * evaluate_a(lst[2])
    else:
        return evaluate_a([evaluate_a(lst[0:3])] + lst[3:])

sum = 0
for line in input:
    sum += evaluate_a(cvrt(line))
silver = sum
print(f"Silver: {silver}")

def evaluate_b(lst, checked=False) -> int:
    if isinstance(lst, int):
        return lst
    elif len(lst) == 1:
        return evaluate_b(lst[0])
    elif len(lst) == 3:
        if lst[1] == "+":
            return evaluate_b(lst[0]) + evaluate_b(lst[2])
        elif lst[1] == "*":
            return evaluate_b(lst[0]) * evaluate_b(lst[2])

    i = 0
    while i < len(lst):
        if lst[i] == "+":
            l = lst[:i-1]
            r = lst[i+2:]
            lst = l + [evaluate_b(lst[i-1:i+2])] + r
        else:
            i += 1

    i = 0
    while i < len(lst):
        if lst[i] == "*":
            l = lst[:i-1]
            r = lst[i+2:]
            lst = l + [evaluate_b(lst[i-1:i+2])] + r
        else:
            i += 1
    assert len(lst) == 1
    return lst[0]

sum = 0
for line in input:
    sum += evaluate_b(cvrt(line))
gold = sum
print(f"Gold: {gold}")
