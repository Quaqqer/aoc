#!/bin/python3

import re
from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=18);
input = puzzle.input_data.split("\n")

num = re.compile("[0-9]+")
def strtolist(curr: str):
    curr = curr.replace(" ", "")
    result = []
    while (curr != ""):
        number = num.match(curr)
        if number:
            result.append(int(number.group(0)))
            curr = curr[len(number.group(0)):]
        elif curr[0] == "(":
            depth = 1
            pos = -1
            for i in range(1, len(curr)):
                if curr[i] == "(":
                    depth += 1
                elif curr[i] == ")":
                    depth -= 1
                    if depth == 0:
                        pos = i
                        break
            result.append(strtolist(curr[1:pos]))
            curr = curr[pos+1:]
        elif curr[0] == "*":
            result.append("*")
            curr = curr[1:]
        elif curr[0] == "+":
            result.append("+")
            curr = curr[1:]
    return result

def eval_a(expr) -> int:
    if isinstance(expr, int):
        return expr
    elif len(expr) == 1:
        return eval_a(expr[0])
    elif len(expr) == 3:
        if expr[1] == "+":
            return eval_a(expr[0]) + eval_a(expr[2])
        elif expr[1] == "*":
            return eval_a(expr[0]) * eval_a(expr[2])
    return eval_a([eval_a(expr[0:3])] + expr[3:])

silver = sum(eval_a(strtolist(line)) for line in input)
print(f"Silver: {silver}")

def eval_b(expr) -> int:
    if isinstance(expr, int):
        return expr
    elif len(expr) == 1:
        return eval_b(expr[0])
    elif len(expr) == 3:
        if expr[1] == "+":
            return eval_b(expr[0]) + eval_b(expr[2])
        elif expr[1] == "*":
            return eval_b(expr[0]) * eval_b(expr[2])

    i = 0
    while i < len(expr):
        if expr[i] == "+":
            l = expr[:i-1]
            r = expr[i+2:]
            expr = l + [eval_b(expr[i-1:i+2])] + r
        else:
            i += 1

    i = 0
    while i < len(expr):
        if expr[i] == "*":
            l = expr[:i-1]
            r = expr[i+2:]
            expr = l + [eval_b(expr[i-1:i+2])] + r
        else:
            i += 1
    return expr[0]

gold = sum(eval_b(strtolist(line)) for line in input)
print(f"Gold: {gold}")
