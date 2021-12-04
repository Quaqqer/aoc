#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 4)
lines = puzzle.input_data.split("\n")

# Main code

calls = [int(v) for v in lines[0].split(",")]

class BingoBoard:
    def __init__(self, strs):
        self.board = [[int(v) for v in row.split()] for row in strs]
        self.marked = [[False for _ in range(5)] for _ in range(5)]
        self.winner = False

    def mark(self, num):
        for row in range(len(self.board)):
            for col in range(len(self.board[row])):
                if num == self.board[row][col]:
                    self.marked[row][col] = True

                    if self.winner or all(self.marked[row][c] for c in range(5)) or all(self.marked[r][col] for r in range(5)):
                        self.winner = True

    def __str__(self):
        return "\n".join(" ".join(str(v) for v in row) for row in self.board)

    def score(self, last_call):
        s = 0
        for row in range(len(self.board)):
            for col in range(len(self.board[row])):
                if not self.marked[row][col]:
                    s += self.board[row][col]
        return s * last_call

boardlines = lines[1:]
boards = []

while len(boardlines) > 0:
    boards.append(BingoBoard(boardlines[1:6]))
    boardlines = boardlines[6:]

def bingo_a(calls):
    ret_score = None
    while len(calls) > 0:
        call = calls[0]
        calls = calls[1:]

        for board in boards:
            board.mark(call)


            if board.winner:
                ret_score = board.score(call)
                boards.remove(board)

        if ret_score is not None:
            return ret_score

def bingo_b(calls):
    while len(calls) > 0:
        call = calls[0]
        calls = calls[1:]

        dels = []
        
        for board in boards:
            board.mark(call)

            if board.winner:
                if len(boards) == 1:
                    return board.score(call)
                dels.append(board)

        for dell in dels:
            boards.remove(dell)

silver = bingo_a(calls)
gold = bingo_b(calls)

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}") # type: ignore
    puzzle.answer_a = silver # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}") # type: ignore
    puzzle.answer_b = gold # type: ignore
