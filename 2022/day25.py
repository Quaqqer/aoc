# This is only part of the solution, I decided to just count it manually
#
# 0 => 0
# 1 => 1
# 2 => 2
# 3 => 1=
# 4 => 1-

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("25"))
id = puzzle.input_data


def snafu(s):
    ans = 0
    for c in s:
        ans *= 5
        match c:
            case "=":
                ans -= 2
            case "-":
                ans -= 1
            case x:
                ans += int(x)
    return ans


def to_base_5(n):
    s = ""
    while n != 0:
        print(n)
        s = str(n % 5) + s
        n //= 5
    return s


num = sum(map(snafu, id.splitlines()))
