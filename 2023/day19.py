# 19   00:29:05  1530      0   02:06:17  2608      0
# Fun problem, it was interesting! Took a while to realize how to cut the 4d
# boxes and I also forgot to take the complement when a rule didn't match. For
# instance if a rule requires a < 10, using the next rule requires a >= 10.

from math import prod
from typing import TypeAlias, cast

from aocd.models import Puzzle

from util import *

Point4: TypeAlias = tuple[int, int, int, int]
Box4: TypeAlias = tuple[Range, Range, Range, Range]

Rule: TypeAlias = tuple[int, str, int, str]
Workflow: TypeAlias = list[Rule | str]

puzzle = Puzzle(2023, int("19"))
data = puzzle.input_data

workflow_strings, point_strings = [s.splitlines() for s in data.split("\n\n")]

points: list[Point4] = [cast(Point4, tuple(ints(l))) for l in point_strings]


def get_workflows() -> dict[str, Workflow]:
    workflows = {}
    """A dictionary of workflows

    Keys are workflow names and values are either a tuple containing
    (dimension, operator, n, destination) or fallback destination.
    """
    dimensions = {"x": 0, "m": 1, "a": 2, "s": 3}
    for wfs_ in workflow_strings:
        name, rest = wfs_.split("{")
        rest = rest[:-1]
        rests = rest.split(",")

        workflows[name] = []

        for rest in rests:
            if "<" in rest or ">" in rest:
                op = "<" if "<" in rest else ">"
                var, rest_ = rest.split(op)
                n, dest = rest_.split(":")
                workflows[name].append((dimensions[var], op, int(n), dest))
            else:
                workflows[name].append(rest)

    return workflows


def test(v: Point4, workflows: dict[str, Workflow], workflow="in") -> bool:
    match workflow:
        case "A":
            return True
        case "R":
            return False

    for rule in workflows[workflow]:
        match rule:
            case str(dest):
                return test(v, workflows, dest)
            case (dim, op, n, dest):
                if eval(f"{v[dim]} {op} {n}"):
                    return test(v, workflows, dest)
    assert False


def search(workflows: dict[str, Workflow]):
    queue: list[tuple[str, Box4]] = [
        ("in", (Range(1, 4001), Range(1, 4001), Range(1, 4001), Range(1, 4001)))
    ]

    found_boxes: list[Box4] = []

    while queue:
        rule, v = queue.pop()

        match rule:
            case "A":
                found_boxes.append(v)
                continue
            case "R":
                continue

        for cond in workflows[rule]:
            match cond:
                case str(dest):
                    queue.append((dest, v))
                case (i, op, n, dest):
                    rule_range = Range(1, n) if op == "<" else Range(n + 1, 4001)
                    complement_range = Range(n, 4001) if op == "<" else Range(1, n + 1)
                    if (o := v[i].overlap(rule_range)) is not None:
                        queue.append((dest, cast(Box4, v[:i] + (o,) + v[i + 1 :])))

                    match v[i].overlap(complement_range):
                        case None:
                            continue
                        case o:
                            v = cast(Box4, v[:i] + (o,) + v[i + 1 :])

    dest = 0

    for i, box in enumerate(found_boxes):
        boxes: list[Box4] = [box]

        for prev_box in found_boxes[:i]:
            boxes = flatten([cut_box(rs, prev_box) for rs in boxes])

        dest += sum(prod(r.len() for r in box) for box in boxes)

    return dest


def cut_range(rng: Range, cutter: Range) -> list[Range]:
    rl = None
    if 1 < cutter.start:
        rl = Range(1, cutter.start).overlap(rng)
    rr = None
    if cutter.end < 4001:
        rr = Range(cutter.end, 4001).overlap(rng)

    return [rng for rng in [rl, rr] if rng is not None]


def cut_box(box: Box4, cutter: Box4) -> list[Box4]:
    sub_boxes: list[Box4] = []

    overlap: tuple[Range, ...] = tuple()

    for i in range(4):
        sub_boxes += [
            cast(Box4, (*overlap, cut, *box[i + 1 :]))
            for cut in cut_range(box[i], cutter[i])
        ]

        overlap_range = box[i].overlap(cutter[i])

        if overlap_range is None:
            return sub_boxes

        overlap += (overlap_range,)

    return sub_boxes


workflows = get_workflows()
puzzle.answer_a = sum(sum(p) for p in points if test(p, workflows))
puzzle.answer_b = search(workflows)
