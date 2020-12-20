import typing
from collections import Counter, defaultdict
from math import prod
from typing import DefaultDict, Iterable, List, Dict, Optional, Set, Tuple, TypeVar

# s = open("day_20/tw.txt").read().strip()
s = open("day_20/ex.txt").read().strip()

tiles = s.strip().split("\n\n")

tile_map = {}

for (tid, *glob) in map(str.splitlines, tiles):
    assert tid.startswith("Tile ")
    tile_map[tid[5:-1]] = glob

flipped: Dict[str, str] = {}
tile_edges: DefaultDict[str, List[str]] = defaultdict(list)
edge_map: DefaultDict[str, Set[str]] = defaultdict(set)

for tid, body in tile_map.items():
    parts: List[str] = [
        body[0],
        [first for first, *_ in body],  # type: ignore
        [last for *_, last in body],  # type: ignore
        body[-1],
    ]

    for edge in map("".join, parts):
        inverted = edge[::-1]

        tile_edges[tid].extend([inverted, edge])

        edge_map[inverted].add(tid)
        edge_map[edge].add(tid)


def count(item: Tuple[str, List[str]]) -> Optional[int]:
    tid, edges = item

    edges_: List[Set[str]] = [edge_map[edge] for edge in edges]

    c = Counter(map(len, (edge ^ {tid} for edge in edges_)))

    if c[0] == c[1] == 4:
        return int(tid)
    else:
        return None


corners = set(filter(None, map(count, tile_edges.items())))

print(f"Part1: {prod(corners)}")
