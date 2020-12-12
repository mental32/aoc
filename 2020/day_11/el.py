from collections import defaultdict
from pathlib import Path
from typing import (
    Callable,
    Dict,
    List, NamedTuple,
    Optional, Set,
    Tuple,
)

p = Path(__file__).parent.joinpath("el.txt").absolute()


def parse(p: Path) -> Tuple["SeatPlan", range, range]:
    assert p.exists(), p
    st = p.read_text().strip().split("\n")

    rows = range(len(st))
    cols = range(len(st[0]))

    body = {
        (x, y): {".": FLOOR, "L": EMPTY, "#": OCCUPIED}[c]
        for x, row in enumerate(st)
        for y, c in enumerate(row)
    }

    seats = {xy for xy, cell in body.items() if cell == EMPTY}

    return (SeatPlan(body, seats), rows, cols)


Fn = Callable[[Tuple[int, int]], int]

north = (-1, 0)
south = (+1, 0)
east = (0, +1)
west = (0, -1)
southwest = (+1, -1)
southeast = (+1, +1)
northwest = (-1, -1)
northeast = (-1, +1)

COMPASS_FACES = (
    north,
    south,
    east,
    west,
    southeast,
    southwest,
    northwest,
    northeast,
)

FLOOR = 0
EMPTY = 1
OCCUPIED = 2

class SeatPlan(NamedTuple):
    cache: Dict[Tuple[int, int], int]
    seats: Set[Tuple[int, int]]

    def n_occupied(self) -> int:
        return sum([kind == OCCUPIED for kind in self.cache.values()])

    @staticmethod
    def stablize(
        initial: "SeatPlan",
        occupation_threshold: int,
        adjacent_cb: Callable[["SeatPlan", int, int], List[Tuple[int, int]]],
    ) -> "SeatPlan":
        from copy import deepcopy

        plan = deepcopy(initial)
        flow = SeatPlan({}, initial.seats)

        diffs: int
        diffs = 0

        def on_empty(xy: Tuple[int, int]) -> int:
            nonlocal diffs, plan

            (col, row) = xy

            for xy in adjacent_cb(plan, col, row):
                if plan.cache[xy] == OCCUPIED:
                    return EMPTY
            else:
                diffs += 1
                return OCCUPIED

        def on_occupied(xy: Tuple[int, int]) -> int:
            nonlocal diffs, plan

            (col, row) = xy
            acc = 0

            for xy in adjacent_cb(plan, col, row):
                acc += plan.cache[xy] == OCCUPIED

                if acc >= occupation_threshold:
                    diffs += 1
                    return EMPTY
            else:
                return OCCUPIED

        registry: Tuple[Fn, Fn, Fn] = (
            (lambda _: FLOOR),
            on_empty,
            on_occupied,
        )

        while True:
            diffs -= diffs

            flow.cache.clear()

            for xy, cell in plan.cache.items():
                flow.cache[xy] = registry[cell](xy) if cell != FLOOR else FLOOR

            if not diffs:
                return flow

            plan.cache.update(flow.cache)
            flow.cache.clear()

    @staticmethod
    def stablize_fast(
        initial: "SeatPlan",
        occupation_threshold: int,
        adjacent_cb: Callable[["SeatPlan", int, int], List[Tuple[int, int]]],
    ) -> int:
        empty: Set[Tuple[int, int]] = set()
        occupied = initial.seats.copy()

        neighbors = {
            (x, y): adjacent_cb(initial, x, y)
            for (x, y) in initial.cache
        }

        while True:
            occupy, deoccupy = set(), set()

            for seat in empty:
                for neighbor in neighbors[seat]:
                    if neighbor in occupied:
                        break
                else:
                    occupy.add(seat)

            for seat in occupied:
                acc = 0
                for neighbor in neighbors[seat]:
                    acc += neighbor in occupied
                    if acc >= occupation_threshold:
                        deoccupy.add(seat)
                        break

            if not occupy and not deoccupy:
                break

            occupied = (occupied - deoccupy) | occupy
            empty = (empty - occupy) | deoccupy

        return len(occupied)


def part1(initial: SeatPlan) -> int:
    def _(_: SeatPlan, x: int, y: int, _cache = {}) -> List[Tuple[int, int]]:
        try:
            return _cache[(x, y)]
        except KeyError:
            key = (x, y)

        raw = [
            (x, y - 1),  # left
            (x, y + 1),  # right
            (x + 1, y),  # down
            (x - 1, y),  # up
            (x + 1, y - 1),  # bottom-left
            (x + 1, y + 1),  # bottom-right
            (x - 1, y - 1),  # top-left
            (x - 1, y + 1),  # top-right
        ]

        neighbours = [
            xy
            for xy in raw
            if xy in _.seats
        ]

        _cache[key] = neighbours

        return neighbours

    return SeatPlan.stablize_fast(initial, occupation_threshold=4, adjacent_cb=_)


def part2(initial: SeatPlan):
    def _(plan: SeatPlan, x: int, y: int, _cache = {}) -> List[Tuple[int, int]]:
        try:
            return _cache[(x, y)]
        except KeyError:
            key = (x, y)

        # The neighbours are the "first-seen" in the adjacent axis.
        #
        # For instance, the following empty seat is our center and the occupied
        # seats are the neighbours:
        #
        # .......#.
        # ...#.....
        # .#.......
        # .........
        # ..#L....#
        # ....#....
        # .........
        # #........
        # ...#.....
        def trace(delta: Tuple[int, int]) -> Optional[Tuple[int, int]]:
            """Trace a line on a `grid` from some `origin` translating by some `delta` until `pred` returns true."""
            [dx, dy] = delta
            [ix, iy] = (x, y)

            while True:
                ix += dx
                iy += dy

                pair = (ix, iy)

                try:
                    if plan.cache[pair] != FLOOR:
                        return pair
                except KeyError:
                    break

            return None

        raw = [trace(direction) for direction in COMPASS_FACES]

        _cache[key] = results = list(filter(None, raw))
        return results

    return SeatPlan.stablize_fast(initial, occupation_threshold=5, adjacent_cb=_)

(inf, ROWS, COLS) = parse(p)

print(f"Part1: {part1(inf)}")
print(f"Part2: {part2(inf)}")
