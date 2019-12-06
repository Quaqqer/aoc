import sys
import math
import pygame
import pygame.locals
import time
pygame.init()
screen = pygame.display.set_mode((1000, 1000))

wire_map = {}
wire_map2 = {}
cross_sections = {}

min_x = 0
min_y = 0
max_x = 0
max_y = 0

def create_wire(wire_map, moves):
    x, y = 0, 0
    mode = 1 if wire_map else 0
    steps = 0 
    color = (0, 255, 0) if mode == 0 else (255, 0, 0)
    for move in moves:
        drct = move[0]
        amt = int(move[1:])

        global max_x
        global max_y
        global min_y
        global min_x

        for i in range(amt):
            if drct == "U":
                y += 1
            elif drct == "D":
                y -= 1
            elif drct == "L":
                x -= 1
            elif drct == "R":
                x += 1

            if x > max_x:
                max_x = x
            if x < min_x:
                min_x = x
            if y > max_y:
                max_y = y
            if y < min_y:
                min_y = y

            steps += 1
            if mode == 1:
                wire_map2[(x, y)] = steps
                if (x, y) in wire_map:
                    other_steps = wire_map[(x, y)]
                    if (x, y) not in cross_sections:
                        cross_sections[(x, y)] = steps + other_steps
            else:
                if (x, y) not in wire_map:
                    wire_map[(x, y)] = steps

for line in sys.stdin:
    if line.endswith("\n"):
        line = line[:-1]

    moves = line.split(",")

    create_wire(wire_map, moves)

shortest_dist = -1

for pos in cross_sections:
    dist = abs(pos[0]) + abs(pos[1])
    if shortest_dist == -1 or dist < shortest_dist:
        shortest_dist = dist

print(shortest_dist)

fewest_steps = -1

for pos in cross_sections:
    steps = cross_sections[pos]
    if fewest_steps == -1 or steps < fewest_steps:
        fewest_steps = steps

print(fewest_steps)

max_x += 5
min_x -= 5
max_y += 5
min_y -= 5

for pos in wire_map:
    x, y = pos
    sx = int(((x - min_x)/(max_x - min_x)) * 1000)
    sy = int(((y - min_y)/(max_y - min_y)) * 1000)
    screen.set_at((sx, sy), (0, 255, 0))

for pos in wire_map2:
    x, y = pos
    sx = int(((x - min_x)/(max_x - min_x)) * 1000)
    sy = int(((y - min_y)/(max_y - min_y)) * 1000)
    screen.set_at((sx, sy), (255, 0, 0))

x = int(((0 - min_x)/(max_x - min_x)) * 1000)
y = int(((0 - min_y)/(max_y - min_y)) * 1000)
pygame.draw.circle(screen, (0, 0, 255), (x, y), 3)

pygame.display.flip()

running = True
while running:
    time.sleep(1)
    for event in pygame.event.get():
        if event.type == pygame.locals.QUIT:
            running = False
        if event.type == pygame.locals.KEYDOWN:
            if event.key == pygame.locals.K_ESCAPE:
                running = False
