# 24   00:15:03   872      0   02:19:34  1076      0
# Graphviz pog!
#
# My initial solution was to check the graph with graphviz. Afterwards I looked
# at Hyper Neutrino's video, (https://www.youtube.com/watch?v=SU6lp6wyd3I). On
# christmas day I implemented it. Smart solution!


import tempfile
from typing import Iterable, cast

from aocd.models import Puzzle
from graphviz import Digraph

puzzle = Puzzle(2024, int("24"))

data = puzzle.input_data

reg_data, formula_data = data.split("\n\n")
regs = []

init_regs = {}
for line in reg_data.splitlines():
    reg, v = line.split(": ")
    init_regs[reg] = int(v)


def k(r0: str, r1: str) -> tuple[str, str]:
    return cast(tuple[str, str], tuple(sorted((r0, r1))))


type Formulas = list[tuple[str, str, str, str]]

formulas: Formulas = []
for line in formula_data.splitlines():
    a, op, b, _, res = line.split()
    formulas.append((a, op, b, res))


def run(init_regs: dict[str, int], formulas: Formulas) -> dict[str, int] | None:
    vals = init_regs.copy()
    done = False
    while not done:
        done = True
        done_ops = 0
        for formula in formulas:
            match formula:
                case a, "AND", b, res:
                    if res not in vals:
                        done = False
                        if a in vals and b in vals:
                            vals[res] = int(vals[a] and vals[b])
                            done_ops += 1
                case a, "XOR", b, res:
                    if res not in vals:
                        done = False
                        if a in vals and b in vals:
                            vals[res] = int(vals[a] ^ vals[b])
                            done_ops += 1
                case a, "OR", b, res:
                    if res not in vals:
                        done = False
                        if a in vals and b in vals:
                            vals[res] = int(vals[a] or vals[b])
                            done_ops += 1
                case _:
                    raise Exception()
        if not done and done_ops == 0:
            return None
    return vals


def get_var(regs: dict[str, int], var: str) -> int:
    out = 0
    for i in range(100)[::-1]:
        reg = f"{var}{i:02}"
        if reg in regs:
            out = out << 1 | regs[reg]
    return out


def solve_a() -> int:
    regs = run(init_regs, formulas)
    assert regs is not None
    return get_var(regs, "z")


def view_graph(formulas: Formulas):
    g = Digraph("AoC day 24 adder")

    for i, (a, op, b, res) in enumerate(formulas):
        op_id = f"_{op}{i}"
        g.node(op_id, op)
        g.edge(a, op_id)
        g.edge(b, op_id)
        g.edge(op_id, res)

    f = tempfile.mktemp(".svg")
    g.render(f.removesuffix(".svg"), format="svg", engine="dot", view=True)


def invalids(formulas: Formulas) -> tuple[tuple[int, int], list[str]] | None:
    ands: dict[tuple[str, str], str] = {}
    xors: dict[tuple[str, str], str] = {}
    ors: dict[tuple[str, str], str] = {}

    for a, op, b, res in formulas:
        d = {"AND": ands, "OR": ors, "XOR": xors}[op]
        d[k(a, b)] = res

    bits = 0
    while f"x{bits + 1:02}" in init_regs:
        bits += 1

    if xors[k("x00", "y00")] != "z00":
        return ((0, 0), ["z00"])

    carry = ands[k("x00", "y00")]
    for i in range(1, bits + 1):
        x, y, z = f"x{i:02}", f"y{i:02}", f"z{i:02}"
        zi = xors[k(x, y)]
        if k(zi, carry) not in xors:
            return (i, 0), [zi]

        z_expected = xors[k(zi, carry)]
        if z != z_expected:
            return ((i, 1), [zi, z_expected])

        ci0 = ands[k(x, y)]
        if k(zi, carry) not in ands:
            return ((i, 2), [zi])
        ci1 = ands[k(zi, carry)]
        if k(ci0, ci1) not in ors:
            return ((i, 3), [ci0, ci1])
        carry = ors[k(ci0, ci1)]

    return None


def swap_formulas(
    formulas: Formulas, to_swap: str
) -> Iterable[tuple[list[tuple[str, str, str, str]], list[str]]]:
    [i] = [i for i in range(len(formulas)) if formulas[i][-1] == to_swap]

    for j in range(len(formulas)):
        if i == j:
            continue

        f1 = formulas[i]
        f2 = formulas[j]
        nf1 = f1[:-1] + f2[-1:]
        nf2 = f2[:-1] + f1[-1:]
        nfs = [formulas[k] for k in range(len(formulas)) if k != i and k != j] + [
            nf1,
            nf2,
        ]
        swaps = [f1[-1], f2[-1]]
        yield nfs, swaps


def solve_b(formulas: Formulas) -> str:
    """Assumes that there is just one single swap per adder"""

    swaps = []
    iv = invalids(formulas)
    while iv is not None:
        pos, possible_wrongs = iv

        for nfs, swapped in (
            v for pw in possible_wrongs for v in swap_formulas(formulas, pw)
        ):
            new_iv = invalids(nfs)
            if new_iv is None or pos < new_iv[0]:
                formulas = nfs
                swaps += swapped
                break
        else:
            assert False, f"Could not find swap for {possible_wrongs}"

        iv = invalids(formulas)

    return ",".join(sorted(swaps))


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b(formulas)
