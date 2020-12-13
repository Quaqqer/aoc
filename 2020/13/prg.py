#!/bin/python3

from aocd.models import Puzzle
import math

puzzle = Puzzle(year=2020, day=13);
input = puzzle.input_data.split("\n")

time = int(input[0])
busses = [int(i) for i in input[1].split(",") if i.isdigit()]

lst = [(-(time % bus) + bus, bus) for bus in busses]
print(lst)
lowestDelta, bus = min(lst)

silver = lowestDelta * bus
print(f"Silver: {silver}")

def lcm(a, b):
    return abs(a*b) //  math.gcd(a, b)

def lcml(lst):
    if not lst:
        return 1
    return lcm(lst[0], lcml(lst[1:]))

# chinese remainder theoreom
t = 0
dt = 0
mods = []
for bus in input[1].split(","):
    if bus != "x":
        v = int(bus)
        _lcm = lcml(mods)
        while (t + dt) % v != 0:
            t += _lcm
        mods.append(v)
    dt += 1

gold = t
print(f"Gold: {gold}")
