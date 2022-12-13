# 13   00:40:42   2810      0   00:51:38   2694      0
# Pretty slow today, got stuck on equal comparisons between lists

import math

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("13"))
id = puzzle.input_data


# Main code
to_compare = [
    tuple(eval(line) for line in chunk.splitlines()) for chunk in id.split("\n\n")
]


def compare(left, right) -> None | bool:
    match left, right:
        case int(l), int(r):
            if l == r:
                return None
            else:
                return l < r

        case int(l), list(r):
            return compare([l], r)

        case list(l), int(r):
            return compare(l, [r])

        case [], []:
            return None

        case [], [rhead, *rtail]:
            return True

        case [lhead, *ltail], []:
            return False

        case [lhead, *ltail], [rhead, *rtail]:
            head_comparison = compare(lhead, rhead)

            if head_comparison is None:
                return compare(ltail, rtail)
            else:
                return head_comparison


puzzle.answer_a = sum(i + 1 for i, (l, r) in enumerate(to_compare) if compare(l, r))


class Packet:
    def __init__(self, line):
        self.data = eval(line)

    def __lt__(self, other):
        return compare(self.data, other.data) == True


find_packets = [Packet("[[2]]"), Packet("[[6]]")]
all_packets = [Packet(line) for line in id.splitlines() if line] + find_packets
sorted_packets = sorted(all_packets)
puzzle.answer_b = math.prod(sorted_packets.index(p) + 1 for p in find_packets)
