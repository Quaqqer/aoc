from __future__ import annotations

import dataclasses
import itertools
from dataclasses import dataclass
from typing import Iterable, Iterator

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2015, int("21"))

data = puzzle.input_data


@dataclass(frozen=True)
class Stats:
    hp: int
    damage: int
    armor: int

    def add_item(self, item: Item) -> Stats:
        return Stats(self.hp, self.damage + item.damage, self.armor + item.armor)

    def add_items(self, items: Iterable[Item]) -> Stats:
        stats = self
        for item in items:
            stats = stats.add_item(item)
        return stats


@dataclass(frozen=True)
class Item:
    cost: int
    damage: int
    armor: int


def parse_item(s: str) -> Item:
    *_, cost, damage, armor = ints(s)
    return Item(cost, damage, armor)


shop = """Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3"""

weapons_s, armors_s, rings_s = shop.split("\n\n")

weapons = [parse_item(l) for l in weapons_s.splitlines()[1:]]
armors = [parse_item(l) for l in armors_s.splitlines()[1:]]
rings = [parse_item(l) for l in rings_s.splitlines()[1:]]


def item_combinations() -> Iterator[tuple[Item, ...]]:
    for weapon in weapons:
        for armor in armors + [Item(0, 0, 0)]:
            for n_rings in [0, 1, 2]:
                for rings_ in itertools.combinations(rings, n_rings):
                    yield weapon, armor, *rings_


def fight(player: Stats, boss: Stats) -> bool:
    while True:
        boss = dataclasses.replace(
            boss, hp=boss.hp - max(player.damage - boss.armor, 0)
        )
        if boss.hp <= 0:
            return True
        player = dataclasses.replace(
            player, hp=player.hp - max(boss.damage - player.armor, 0)
        )
        if player.hp <= 0:
            return False


hp, damage, armor = ints(data)
base_player = Stats(100, 0, 0)
boss = Stats(hp, damage, armor)

puzzle.answer_a = min(
    sum(item.cost for item in items)
    for items in item_combinations()
    if fight(base_player.add_items(items), boss)
)

puzzle.answer_b = max(
    sum(item.cost for item in items)
    for items in item_combinations()
    if not fight(base_player.add_items(items), boss)
)
