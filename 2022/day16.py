# 16   01:36:48   1397      0   02:06:19    551      0
# Pretty hard problem today, went well though. Overslept by 30 minutes, gotta get my
# sleep in order...

import re
from heapq import heappop, heappush

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("16"))
id = puzzle.input_data
# id = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
# Valve BB has flow rate=13; tunnels lead to valves CC, AA
# Valve CC has flow rate=2; tunnels lead to valves DD, BB
# Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
# Valve EE has flow rate=3; tunnels lead to valves FF, DD
# Valve FF has flow rate=0; tunnels lead to valves EE, GG
# Valve GG has flow rate=0; tunnels lead to valves FF, HH
# Valve HH has flow rate=22; tunnel leads to valve GG
# Valve II has flow rate=0; tunnels lead to valves AA, JJ
# Valve JJ has flow rate=21; tunnel leads to valve II"""


valves = dict()
for line in id.splitlines():
    valve, flow_rate, leads_to = re.match(
        r"Valve (.*) has flow rate=(-?\d+); tunnels? leads? to valves? (.*)", line
    ).groups()
    flow_rate = int(flow_rate)
    leads_to = leads_to.split(", ")
    valves[valve] = (flow_rate, leads_to)


non_empty = sum(flow_rate > 0 for flow_rate, _ in valves.values())


moves = []


def bfs(from_):
    queue = [(from_, 0)]
    visited = {from_}
    possible_moves = []

    while queue:
        curr, time = queue.pop(0)

        _, neighbours = valves[curr]

        for neighbour in neighbours:
            if neighbour not in visited:
                nflow, _ = valves[neighbour]
                queue.append((neighbour, time + 1))
                visited.add(neighbour)
                if nflow != 0:
                    possible_moves.append((nflow, time + 1, neighbour))

    return possible_moves


moves = {}

for from_, (flow, _) in valves.items():
    if flow > 0 or from_ == "AA":
        to = bfs(from_)
        moves[from_] = to


def dijkstra(start):
    queue = [(0, start, set(), 0)]

    best = 0

    while queue:
        time, pos, opened, released = heappop(queue)

        best = max(best, released)

        for flow, cost, next in moves[pos]:
            nexttime = time + cost + 1
            if nexttime <= 30 and next not in opened:
                heappush(
                    queue,
                    (
                        nexttime,
                        next,
                        opened | {next},
                        released + flow * (30 - nexttime),
                    ),
                )

    return best


def dijkstra2(start):
    i = 0
    queue = [(0, 0, start, 0, start, set(), 0)]

    best = 0

    while queue:
        (
            time,
            stime,
            spos,
            etime,
            epos,
            opened,
            released,
        ) = heappop(queue)

        if i % 10000 == 0:
            print(time)
        i += 1

        best = max(best, released)
        # We can probably prune this, lol
        if released < best - 500:
            continue

        spots = []
        if time == stime:
            for flow, cost, next in moves[spos]:
                snexttime = stime + cost + 1
                if snexttime <= 26 and next not in opened:
                    spots.append((snexttime, next, {next}))
        else:
            spots = [(stime, spos, set())]

        epots = []
        if time == etime:
            for flow, cost, next in moves[epos]:
                enexttime = etime + cost + 1
                if enexttime <= 26 and next not in opened:
                    epots.append((enexttime, next, {next}))
        else:
            epots = [(etime, epos, set())]

        for snexttime, snext, sopen in spots:
            for enexttime, enext, eopen in epots:
                if len(sopen & eopen) != 1:
                    nexttime = min(snexttime, enexttime)
                    nextreleased = released

                    if sopen:
                        flow = sum(valves[o][0] for o in sopen)
                        nextreleased += flow * (26 - snexttime)
                    if eopen:
                        flow = sum(valves[o][0] for o in eopen)
                        nextreleased += flow * (26 - enexttime)

                    heappush(
                        queue,
                        (
                            nexttime,
                            snexttime,
                            snext,
                            enexttime,
                            enext,
                            opened | sopen | eopen,
                            nextreleased,
                        ),
                    )

    return best


ans_a = dijkstra("AA")
print(ans_a)

puzzle.answer_a = ans_a

ans_b = dijkstra2("AA")
print(ans_b)
puzzle.answer_b = ans_b
