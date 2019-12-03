from re import finditer
from typing import Tuple, Iterator, Dict

with open("input_data") as file:
    left, right, *_ = file.readlines()

assert left.count(",") == right.count(",")

x_ = dict(zip("LRUD", (-1, 1, 0, 0)))
y_ = dict(zip("LRUD", (0, 0, 1, -1)))

def points_for(some: str) -> Iterator[Tuple[int, int, int]]:
    x = y = 0
    steps = 0

    for match in finditer(r"([^,]+)", some):
        element = match[0]
        direction, magnitude = element[:1], element[1:]

        for _ in range(int(magnitude)):
            x += x_[direction]
            y += y_[direction]
            steps += 1
            yield (x, y, steps)

# Part 1

mapl = {tuple(xy): z for (*xy, z) in points_for(left)}
mapr = {tuple(xy): z for (*xy, z) in points_for(right)}

shared = set(mapl) & set(mapr)
dist = min([abs(x) + abs(y) for x, y in shared])

# Part 2

maps = {xy: (mapl[xy] + mapr[xy]) for xy in shared}
smol = min(maps.values())

print(smol)
