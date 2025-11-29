# pyright: basic

import functools
import itertools
import math
import re
from collections import Counter, defaultdict, deque
from copy import copy, deepcopy
from dataclasses import dataclass

import util
from aocd.models import Puzzle
from util import Vec2, Vec3, ints

puzzle = Puzzle(2025, int("01"))

data = puzzle.input_data

lines = data.splitlines()

for line in lines:
    pass
