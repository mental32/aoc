from collections import defaultdict
from re import findall

INPUT_DATA = range(353096, 843212)

from concurrent.futures import ThreadPoolExecutor


def match(code: str) -> bool:
    last = -1
    counter: Dict[str, int] = defaultdict(int)

    for digit in code:
        if (cur := int(digit)) < last:
            return False

        last = cur
        counter[digit] += 1

    return any(value >= 2 for value in counter.values())

ex = ThreadPoolExecutor()
print(sum(ex.map(match, map(str, INPUT_DATA))))
