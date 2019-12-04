from concurrent.futures import ThreadPoolExecutor
from collections import defaultdict
from re import search

INPUT_DATA = range(353096, 843212)

ex = ThreadPoolExecutor()

# Part 1

def match(value: int) -> bool:
    if len((code := str(value))) != 6:
        return False

    last = -1
    doubles = 0
    singles = []

    for digit in code:
        if (cur := int(digit)) < last:
            return False

        last = cur

        if last not in singles:
            singles.append(last)
        else:
            singles.remove(last)
            doubles += 1

    return bool(doubles)

print("Part 1: ...", end="\r", flush=True)
print(f"Part 1: {sum(ex.map(match, INPUT_DATA))}")

# Part 2

def match_more(value: int) -> bool:
    code = str(value)

    if not match(value):
        return False

    return bool(search(r"(^|(.)(?!\2))(\d)\3{1}(?!\3)", code))

print("Part 2: ...", end="\r", flush=True)
print(f"Part 2: {sum(ex.map(match_more, INPUT_DATA))}")
