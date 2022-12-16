# 16   01:36:48   1397      0   02:06:19    551      0
# Pretty hard problem today, went well though. Overslept by 30 minutes, gotta
# get my sleep in order...

import re
from heapq import heappop, heappush

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("16"))
id = puzzle.input_data


valves: dict[str, tuple[int, str]] = {}
for line in id.splitlines():
    valve, flow_rate, leads_to = re.match(
        r"Valve (.*) has flow rate=(-?\d+); tunnels? leads? to valves? (.*)", line
    ).groups()  # type: ignore
    valves[valve] = (int(flow_rate), leads_to.split(", "))  # type: ignore


non_empty = sum(flow_rate > 0 for flow_rate, _ in valves.values())


def bfs_moves(from_) -> list[tuple[int, int, str]]:
    """
    Find moves from a position, this is way better than doing one thing in each
    dijkstra search step, like turning on a valve and moving one step to each
    cave. This way we can do it all in one step.
    """
    queue: list[tuple[str, int]] = [(from_, 0)]
    visited: set[str] = {from_}
    possible_moves: list[tuple[int, int, str]] = []

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


# Construct moves
moves = {from_: bfs_moves(from_) for from_, _ in valves.items()}


def part_a(start="AA"):
    """
    Dijjkstra search for the best time.
    """
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


def part_b(start="AA"):
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


puzzle.answer_a = part_a()

puzzle.answer_b = part_b()
