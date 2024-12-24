# 24   00:15:03   872      0   02:19:34  1076      0
# Graphviz pog!


import tempfile

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


formulas = []
for line in formula_data.splitlines():
    a, op, b, _, res = line.split()
    formulas.append((a, op, b, res))


def run(
    init_regs: dict[str, int], formulas: list[tuple[str, str, str, str]]
) -> dict[str, int] | None:
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


def view_graph(formulas: list[tuple[str, str, str, str]]):
    g = Digraph("AoC day 24 adder")

    for i, (a, op, b, res) in enumerate(formulas):
        op_id = f"_{op}{i}"
        g.node(op_id, op)
        g.edge(a, op_id)
        g.edge(b, op_id)
        g.edge(op_id, res)

    f = tempfile.mktemp(".svg")
    g.render(f.removesuffix(".svg"), format="svg", engine="dot", view=True)


swaps = []


def swap(r0: str, r1: str):
    global swaps
    swaps = sorted(swaps + [r0, r1])

    [i] = [i for i, v in enumerate(formulas) if v[-1] == r0]
    [j] = [i for i, v in enumerate(formulas) if v[-1] == r1]
    f1 = formulas[i]
    f2 = formulas[j]
    nf1 = f1[:-1] + f2[-1:]
    nf2 = f2[:-1] + f1[-1:]
    formulas[i] = nf1
    formulas[j] = nf2


puzzle.answer_a = get_var(run(init_regs, formulas), "z")

# I have no automatic solution for part 2. Look at the graph and make sure that
# it is a proper adder. Swap the nodes that are wrong manually. There should be
# 4 swaps.

# swap("aaa", "bbb")
# swap("ccc", "ddd")
# swap("eee", "fff")
# swap("ggg", "hhh")
# view_graph(formulas)
# puzzle.answer_b = ','.join(swaps)
