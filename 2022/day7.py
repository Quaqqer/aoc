#!/usr/bin/env python3
import re
from typing import Optional

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("07"))
id = puzzle.input_data

# Main code
cwd = "/"
root = dict()


def put(path, file: Optional[int]):
    curr = root
    for d in path.split("/")[:-1]:
        if d != "":
            if d not in curr:
                curr[d] = dict()
            curr = curr[d]
    curr[path.split("/")[-1]] = file if isinstance(file, int) else dict()


in_ls = False
for line in id.splitlines()[1:]:
    if line.startswith("$"):
        in_ls = False

    if in_ls:
        if m := re.match(r"(\d+) (.*)", line):
            put(cwd + "/" + m.group(2), int(m.group(1)))
        elif m := re.match(r"dir (.*)", line):
            put(cwd + "/" + m.group(1), None)

    if line == "$ ls":
        in_ls = True
    elif m := re.match(r"\$ cd (.*)", line):
        path = m.group(1)

        if path == "..":
            cwd = "/".join(cwd.split("/")[:-1])
        else:
            cwd = cwd + "/" + path


def a(path: dict | int, paths: str) -> tuple[int, int]:
    if isinstance(path, int):
        return (path, 0)
    else:
        s = 0
        ans = 0
        for dpaths, p in path.items():
            ds, dans = a(p, paths + "/" + dpaths)
            ans += dans
            s += ds
        if s <= 100000:
            ans += s
        return (s, ans)


used, ans_a = a(root, "/")
puzzle.answer_a = ans_a

tot_size = 70000000
req_free = 30000000

avail = tot_size - used
missing = req_free - avail


def b(path: dict | int, paths: str) -> tuple[int, Optional[int]]:
    if isinstance(path, int):
        return (path, None)
    else:
        best = None
        s = 0
        for dpaths, p in path.items():
            ds, removal_size = b(p, paths + "/" + dpaths)
            s += ds
            if removal_size is not None and (best is None or removal_size < best):
                best = removal_size
        if best is None and missing <= s:
            best = s
        return (s, best)


tot_size, removal_size = b(root, "/")
puzzle.answer_b = removal_size
