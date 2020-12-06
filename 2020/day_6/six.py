import math
from pathlib import Path
from typing import Iterable, List

p = Path(__file__).parent.joinpath("six.txt").absolute()

assert p.exists()

groups = p.read_text().strip().split("\n\n")

def count_groups(st: str) -> int:
    flat = "".join(st.split("\n"))
    return len(set(flat))

# s = sum(map(count_groups, groups))

# print(s)

def count_groups_corrected(st: str) -> int:
    flat = set(st.replace("\n", ""))
    curved = [set(person) for person in st.split("\n")]

    def f(q: str) -> int:
        return all(q in s for s in curved)

    return sum(map(f, flat))

from timeit import timeit

t = timeit("sum(map(count_groups_corrected, groups))", number=1, globals=globals())
print(t)

# s = sum(map(count_groups_corrected, groups))
# print(s)
