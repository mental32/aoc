from typing import Tuple

with open("input_data") as inf:
    grid = list(inf.readlines())


def points_for(here, there):
    x, y = here
    x_, y_ = there

    points_ = set()

    dx = (0 if (x - x_) == 0 else 1) if (x - x_) <= 0 else -1
    dy = (0 if (y - y_) == 0 else 1) if (y - y_) <= 0 else -1

    cur = here
    from itertools import cycle

    for dx_, dy_ in cycle([(dx, 0), (0, dy)]):
        points_.add(cur)
        x__, y__ = cur

        if (x__ >= x_) and (y__ >= y_):
            break

        if x__ != x_:
            x__ += dx_

        if y__ != y_:
            y__ += dy_

        cur = x__, y__

    return points_


def calc(here: Tuple[int, int], others) -> int:
    amount = 0

    others_ = set(others.keys())
    others_.discard(here)

    for there in others:
        if there == here:
            continue

        points_ = points_for(here, there)
        # breakpoint()

        points_.discard(here)
        points_.discard(there)

        blocked_ = bool(others_.intersection(points_))
        # breakpoint()

        if not blocked_:
            amount += 1

    return amount


asteroids = {(x, y): 0 for y, l in enumerate(grid) for x, c in enumerate(l) if c == "#"}

for pos in asteroids:
    asteroids[pos] = calc(pos, asteroids)

# print(asteroids)
print(max(asteroids.items(), key=(lambda it: it[1])))
