# 20   01:33:25   2482      0   01:39:00   2054      0
# i sleep

from __future__ import annotations

from dataclasses import dataclass

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("20"))
id = puzzle.input_data


@dataclass
class LL:
    val: int
    next: LL | None = None
    prev: LL | None = None

    def __repr__(self):
        return str(self.val)


def parse_elems(id: str, part_2: bool):
    key = 811589153

    elems = []
    for line in id.splitlines():
        elems.append(LL(val=int(line)))

    for e1, e2 in zip(elems, elems[1:]):
        e1.next = e2
        e2.prev = e1
    elems[-1].next = elems[0]
    elems[0].prev = elems[-1]

    if part_2:
        for elem in elems:
            elem.val *= key

    return elems


def find_inserter(elem, elems):
    curr = elem

    i = elem.val % (len(elems) - 1)
    if i < 0:
        for _ in range(-i + 1):
            curr = curr.prev
    else:
        for _ in range(i):
            curr = curr.next
    return curr


def insert(elem, inserter):
    if elem == inserter or inserter.next == elem:
        return

    eprev, enext = elem.prev, elem.next

    inext = inserter.next

    eprev.next = enext
    enext.prev = eprev

    inserter.next = elem
    inext.prev = elem

    elem.next = inext
    elem.prev = inserter


def print_list(elem):
    curr = elem
    while True:
        print(curr, end=", ")
        curr = curr.next
        if curr == elem:
            break
    print()


def mix(elems):
    for curr in elems:
        inserter = find_inserter(curr, elems)
        insert(curr, inserter)


def find_answer(elems):
    zero = [e for e in elems if e.val == 0][0]

    ans = 0
    curr = zero
    for _ in range(3):
        for _ in range(1000):
            curr = curr.next
        ans += curr.val
    return ans


elems_a = parse_elems(id, False)
mix(elems_a)
puzzle.answer_a = find_answer(elems_a)

elems_b = parse_elems(id, True)
for _ in range(10):
    mix(elems_b)
puzzle.answer_b = find_answer(elems_b)
