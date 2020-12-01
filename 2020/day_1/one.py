from threading import Thread
from typing import Any, Callable, Iterator, Set, Tuple
from itertools import permutations
from functools import reduce
from operator import mul


with open("./one_inp.txt") as inf:
    _ = inf.read()

s = [int(p.strip()) for p in _.split("\n") if p]

def algo_hack():
    # That doesn't technically work for all inputs:
    #   if there's 1010 in the input it'll use the same value twice
    for x in s:
        y = 2020 - x

        if y in s:
            print(x, y, x * y)

def bag() -> Set[int]:
    it: Set[int] = {
        reduce(mul, xyz, 1)
        for xyz in permutations(s, 3)
        if sum(xyz) == 2020
    }

    return it    

def iterator() -> Set[int]:
    it: Iterator[Tuple[int, ...]] = filter((lambda xyz: sum(xyz) == 2020), permutations(s, 3))
    res = set()

    for (x, y, z) in it:
        res.add(x * y * z)

    return res


def brute_force() -> Set[int]:
    res = set()

    for x in s:
        for y in s:
            for z in s:
                if z + x + y == 2020:
                    res.add(x * y * z)

    return res

def crop() -> Set[int]:
    res = set()

    for i, x in enumerate(s):
        for j, y in enumerate(s[i + 1:]):
            for k, z in enumerate(s[j + 1:]):
                if x + y + z == 2020:
                    res.add(x * y * z)

    return res

def cropped_hack() -> Set[int]:
    bag = tuple(set(s))
    res = set()

    for i, x in enumerate(bag):
        for y in bag[i + 1:]:
            z = 2020 - x - y
            if z in bag:
                res.add(x * y * z)

    return res

from timeit import timeit

t = (lambda f, n: timeit("f()", globals=locals(), number=n))

results = [(t(f, 1), f.__name__) for f in {cropped_hack, crop, brute_force, iterator}]

for (res, name) in sorted(results):
    print(res, name)
