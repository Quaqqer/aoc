#!/bin/python3

from aocd.models import Puzzle
import copy

puzzle = Puzzle(year=2020, day=11);
seats = puzzle.input_data.split("\n")
seats = [[c for c in line] for line in seats]
seatscopy = copy.deepcopy(seats)

def isin(x, y):
    return 0 <= y < len(seats) and 0 <= x < len(seats[y])

def countneighbours(x, y):
    sum = 0
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            if (not dx == dy == 0) and isin(x + dx, y + dy) and seats[y + dy][x + dx] == '#':
                sum += 1
    return sum

def tick(seats):
    next = copy.deepcopy(seats)
    for y in range(len(seats)):
        for x in range(len(seats[y])):
            seat = seats[y][x]
            if seat == '#':
                neighbours = countneighbours(x, y)
                if neighbours >= 4:
                    next[y][x] = 'L'
            elif seat == 'L':
                neighbours = countneighbours(x, y)
                if neighbours == 0:
                    next[y][x] = '#'
    return next

def prnt(seats):
    for y in range(len(seats)):
        print(seats[y])

oldseats = seats
seats = tick(seats)
while (oldseats != seats):
    oldseats = seats
    seats = tick(seats)

def occupied(seats):
    sum = 0
    for y in seats:
        for x in y:
            if x == '#':
                sum += 1
    return sum

silver = occupied(seats)
print(f"Silver: {silver}")

def countvisible(seats, x, y):
    sum = 0
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            if dx == dy == 0:
                continue

            seen = False
            nx, ny = x + dx, y + dy
            while (isin(nx, ny)):
                if seats[ny][nx] == '#':
                    seen = True
                    break
                elif seats[ny][nx] == 'L':
                    seen = False
                    break
                nx, ny = nx + dx, ny + dy
            if seen: sum += 1
    return sum

def tick2(seats):
    next = copy.deepcopy(seats)
    for y in range(len(seats)):
        for x in range(len(seats[y])):
            seat = seats[y][x]
            if seat == '#':
                neighbours = countvisible(seats, x, y)
                if neighbours >= 5:
                    next[y][x] = 'L'
            elif seat == 'L':
                neighbours = countvisible(seats, x, y)
                if neighbours == 0:
                    next[y][x] = '#'
    return next

seats = seatscopy
oldseats = seats
seats = tick2(seats)
while (oldseats != seats):
    oldseats = seats
    seats = tick2(seats)

gold = occupied(seats)
print(f"Gold: {gold}")
