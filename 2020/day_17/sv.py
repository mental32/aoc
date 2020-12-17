import typing
from collections import Counter
from pathlib import Path
from typing import Iterator, Set, Tuple


p = Path(__file__).parent.joinpath("sv.txt")

Point = Tuple[int, ...]
Space = Set[Point]


def edges(r: int) -> Iterator[Tuple[int, ...]]:
    if r == 1:
        yield from [(-1,), (0,), (1,)]
        return

    for i in (-1, 0, 1):
        yield from [(i, *j) for j in edges(r - 1)]


def parse(path: Path, suffix=(0,)) -> Set[Point]:
    assert path.exists()

    inp = path.read_text().strip().splitlines()

    return set(
        len(suffix) * (0,) + (x, y)  # Make sure the points are padded to the correct resolution.
        for x, __ in enumerate(inp)
        for y, _ in enumerate(__)
        if _ == "#"
    )


def cycle(space: Set[Point]) -> Set[Point]:
    if not space:
        return space

    # get a random element from the set.
    point, *_ = space ^ set()

    # Use the point to derive the resolution
    r = len(point)

    # Hit the edge cache for the resolution.
    #
    # The cache is pre-filled for AoC with 3 and 4
    # resolution edges, but is made to support any n-dimensions
    try:
        _ = EDGE_CACHE[r]
    except KeyError:
        _ = EDGE_CACHE[r] = set(edges(r))

    # Filter out the center.
    edge_set = _ ^ {r * (0,)}

    # For every active cell in real-space, reveal its neighbours.
    #
    # Using `Counter` here lets us have a dict where the keys
    # are points in real-space and the values are how many points from
    # our previous state reference them.
    #
    # Compared to other naive solutions where you update the state of the point
    # based on the number of its own neighbours this avoids all that expensive
    # logic by accumulating all the references a point in real-space has, the
    # points collected here may be dead or alive, we dont filter here.

    # fmt: off
    liveness: typing.Counter[Point]
    liveness = Counter([
        tuple(map(sum, zip(real, edge)))  # create a point in real-space from an edge offset + real center
        for real in space
        for edge in edge_set
    ])
    # fmt: on

    # Produce a set of the points of all alive cells based on the liveness map
    # above.
    #
    # This handles dropping alive cells that die and reviving dead cells that
    # will become alive in this cycle.
    return {
        point
        for point, refcount in liveness.items()
        if refcount == 3 or point in space and refcount == 2
    }


def solve(**kwargs) -> int:
    c = parse(p, **kwargs)

    for _ in range(6):
        c = cycle(c)
    else:
        return len(c)


EDGE_CACHE = {
    3: set(edges(3)),
    4: set(edges(4)),
}

print(f"Part1: {solve(suffix=(0,))}")
print(f"Part2: {solve(suffix=(0, 0))}")
