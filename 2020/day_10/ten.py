from collections import Counter, defaultdict
from pathlib import Path
from typing import (
    List,
    Optional,
    Tuple,
)

p = Path(__file__).parent.joinpath("ten.txt").absolute()


def parse(p: Path) -> List[int]:
    assert p.exists(), p
    st = p.read_text().strip().split("\n")
    return list(map(int, st))


def is_valid_sequence(
    seq_t: List[int],
) -> Optional[Tuple[int, ...]]:
    if not seq_t:
        return None

    assert sorted(seq_t) == seq_t

    stop = len(seq_t)
    top = max(seq_t) + 3
    seq = [0, *seq_t, top]

    voltage_diffs = []

    for idx in range(stop + 1):
        left = seq[idx]
        right = seq[idx + 1]

        if left > right:
            return None

        n = right - left

        if n not in (1, 2, 3):
            return None

        voltage_diffs.append(n)

    return tuple(voltage_diffs)


def part1(input_sequence: List[int]):
    insq = sorted(input_sequence)
    result = is_valid_sequence(insq)
    assert result is not None, "unreachable."
    diffs = Counter(result)
    return diffs[1] * diffs[3]


def part2(seq: List[int]) -> int:
    last, seq = max(seq), sorted(seq)

    paths = defaultdict(int)
    paths[0] = 1

    for jolt in seq:
        _ = (lambda idx: paths[jolt - idx])
        paths[jolt] = sum(map(_, range(1, 4)))

    return paths[last]

inf = parse(p)

print(f"Part1: {part1(inf)}")
print(f"Part2: {part2(inf)}")
