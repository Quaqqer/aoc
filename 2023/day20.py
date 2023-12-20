# 20   00:39:28   569      0   01:55:43  1465      0
# > Hello, yes, I would like a problem with weird cycles please. 🙃

import math
from abc import ABC, abstractmethod
from collections import deque
from typing import cast

from aocd.models import Puzzle

puzzle = Puzzle(2023, int("20"))
data = puzzle.input_data
lines = data.splitlines()


class Module(ABC):
    def __init__(self, name: str, dests: list[str]):
        self.name = name
        self.dests = dests

    @abstractmethod
    def low_pulse(self, src: str):
        ...

    @abstractmethod
    def high_pulse(self, src: str):
        ...


class FlipFlop(Module):
    def __init__(self, name, dests):
        super().__init__(name, dests)
        self.state = False

    def low_pulse(self, src: str):
        if self.state:
            self.state = False
            queue_signals("low", self.name, self.dests)
        else:
            self.state = True
            queue_signals("high", self.name, self.dests)

    def high_pulse(self, src: str):
        pass


class Conjunction(Module):
    def __init__(self, name, dests):
        super().__init__(name, dests)
        self.ins: dict[str, str] = {}

    def low_pulse(self, src: str):
        self.ins[src] = "low"

        if all(v == "high" for v in self.ins.values()):
            queue_signals("low", self.name, self.dests)
        else:
            queue_signals("high", self.name, self.dests)

    def high_pulse(self, src: str):
        self.ins[src] = "high"

        if all(v == "high" for v in self.ins.values()):
            queue_signals("low", self.name, self.dests)
        else:
            queue_signals("high", self.name, self.dests)


class Broadcaster(Module):
    def __init__(self, name, dests):
        super().__init__(name, dests)

    def low_pulse(self, src: str):
        queue_signals("low", self.name, self.dests)

    def high_pulse(self, src: str):
        queue_signals("high", self.name, self.dests)


modules: dict[str, Module] = {}
broadcaster: Broadcaster
for line in lines:
    if line.startswith("broadcaster"):
        name = "broadcaster"
        _, dests = line.split(" -> ")
        dests = dests.split(", ")
        module = Broadcaster(name, dests)
    else:
        t = line[0]
        line = line[1:]
        name, dests = line.split(" -> ")
        dests = dests.split(", ")

        match t:
            case "%":
                module = FlipFlop(name, dests)
            case "&":
                module = Conjunction(name, dests)
            case _:
                assert False

    modules[name] = module

rx_in: str | None = None
for module in modules.values():
    for dest in module.dests:
        if dest == "rx":
            rx_in = module.name
        if dest in modules:
            dest_mod = modules[dest]
            if isinstance(dest_mod, Conjunction):
                dest_mod.ins[module.name] = "low"
assert rx_in is not None

q: deque[tuple[str, str, str]] = deque()


def queue_signals(ty: str, src: str, to: list[str]):
    for dest in to:
        q.append((ty, src, dest))


lows = 0
highs = 0
found = False
i = 0
ins = list(cast(Conjunction, modules[rx_in]).ins.keys())
ins_i: list[int | None] = [None for _ in range(len(ins))]

while not all(i is not None for i in ins_i):
    i += 1

    q.append(("low", "button", "broadcaster"))

    while q:
        ty, src, to = q.popleft()

        match ty:
            case "low":
                lows += 1
                if to in modules:
                    modules[to].low_pulse(src)
            case "high":
                highs += 1

                if to == rx_in:
                    index = ins.index(src)
                    if ins_i[index] is None:
                        ins_i[index] = i

                if to in modules:
                    modules[to].high_pulse(src)
            case _:
                assert False

    if i == 1000:
        puzzle.answer_a = lows * highs

puzzle.answer_b = math.lcm(*cast(list[int], ins_i))
