#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=23);
input = puzzle.input_data.strip()

cups = [int(cup) for cup in input]
low = min(cups)
high = max(cups)
curr = cups[0]

for _ in range(100):
    curri = cups.index(curr)
    pickup = []
    for i in range(3):
        if curri + 1 < len(cups):
            pickup.append(cups.pop(curri + 1))
        else:
            pickup.append(cups.pop(0))

    dest = curr - 1
    while dest in pickup or dest < low:
        dest -= 1
        if dest < low:
            dest = high

    desti = cups.index(dest)
    cups = cups[:desti + 1] + pickup + cups[desti + 1:]

    curr = cups[(cups.index(curr) + 1) % len(cups)]

i = cups.index(1)
silver = "".join([str(c) for c in cups[i + 1:] + cups[:i]])
print(f"Silver: {silver}")

class Node:
    def __init__(self, v):
        self.v = v

cups = [int(cup) for cup in input] + [c + 1 for c in range(len(input), 10**6)]

nodes = {}

root = Node(cups[0])
nodes[root.v] = root

curr = root

for i in range(1, len(cups)):
    node = Node(cups[i])
    nodes[cups[i]] = node
    curr.next = node
    curr = node
curr.next = root

curr = root

for i in range(10**7):
    pickup = [curr.next, curr.next.next, curr.next.next.next]
    pvalues = [n.v for n in pickup]
    curr.next = curr.next.next.next.next

    dest = curr.v - 1
    while dest in pvalues or dest < 1:
        dest -= 1
        if dest < 1:
            dest = 10**6
    dest = nodes[dest]

    temp = dest.next
    dest.next = pickup[0]
    pickup[2].next = temp

    curr = curr.next

while curr.v != 1:
    curr = curr.next

gold = curr.next.v * curr.next.next.v
puzzle.answer_b = gold
