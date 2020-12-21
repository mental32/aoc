import operator
import typing
from itertools import chain
from collections import Counter, defaultdict
from pprint import pformat, pprint
from functools import reduce
from typing import DefaultDict, Dict, List, NewType, Set, Tuple, TypeVar

# s = open("day_20/tw.txt").read().strip()
s = open("day_21/twon.txt").read().strip()

# s = """
# mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
# trh fvjkl sbzzf mxmxvkd (contains dairy)
# sqjhc fvjkl (contains soy)
# sqjhc mxmxvkd sbzzf (contains fish)
# """.strip()

Allergen = NewType("Allergen", str)
Ingredient = NewType("Ingredient", str)

counts: typing.Counter[Ingredient] = Counter()

allergens: Set[Allergen] = set()
ingredients: Set[Ingredient] = set()

unmapped: DefaultDict = defaultdict(set)

for idx, line in enumerate(s.splitlines()):
    assert line.endswith(")"), repr(line)
    line = line[:-1]

    left, right = line.strip().split(" (contains ")

    a: Tuple[Allergen, ...] = tuple(map(Allergen, right.split(", ")))
    b: Set[Ingredient] = set(map(Ingredient, left.split(" ")))

    unmapped[(idx, a)] = b
    allergens.update(a)

    for ingredient in b:
        counts[ingredient] += 1
        ingredients.add(ingredient)

found: Dict[Allergen, Set[Ingredient]] = {_: set() for _ in allergens}

for allergen in allergens:
    search = [value for (_, key), value in unmapped.items() if allergen in key]
    found[allergen] = reduce(operator.and_, search)

assert all(found.values()), pformat(found)

pprint(found)

missing = set(counts.keys()) ^ set(chain.from_iterable(found.values()))

print(sum(counts[ingredient] for ingredient in missing))


def recursion(visited: list, a_i: list) -> list:
    visited_len = len(visited)

    if visited_len == len(a_i):
        return visited

    _, i = a_i[visited_len]
    for ing in i:
        if ing not in visited:
            result = recursion(visited + [ing], a_i)
            if result:
                return result

result = recursion([], list(found.items()))
sorted_mapping = sorted(list(zip([a for a, _ in list(found.items())], result)))
st = ','.join([i for _, i in sorted_mapping])

print(st)
