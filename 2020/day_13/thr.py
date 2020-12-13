(_, __) = """
1009310
19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,599,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,x,x,x,x,x,23,x,x,x,x,x,x,x,761,x,x,x,x,x,x,x,x,x,41,x,x,13
""".strip().split("\n")

uniq = set(__.split(",")) ^ {"x"}
buses = [*map(int, uniq)]

earliest = int(_)

# print(_, buses)

q = []

for bus in buses:
    p = (earliest // bus) + 1
    q.append((p, bus))
    # print(f"{p} * {bus} = {p * bus} ({earliest})")

from itertools import count
from math import prod

t = min(q, key=prod)
(___, bid) = t

print(f"Part1: {(prod(t) - earliest) * bid}")

last = 1
prod_ = 1

for (i, num) in enumerate(map(int, __.replace("x", "0").split(","))):
    if num == 0:
        continue

    # since all numbers are prime and co-prime you can
    # just multiply them together to search for bigger
    # numbers, there are no shared multiplers that you
    # need to keep in mind.

    # So the basic search algorithm is that you are going to
    # search first for the first 2 numbers until you find the
    # first multiple of the cumulative product of numbers so far
    # that has a remainder equal to the remainder you are looking for
    # that means that if your number is N and its sequence is i
    # you are looking for a place where
    # N * prev % 13 == 13 - 1
    # of course you'll need to deal with the - 1 (i) being bigger than num in
    # some cases so you'll need to crop it down to the number
    for x in count(last, prod_):
        if x % num == (num - (i % num)) % num:
            last = x
            prod_ *= num
            break

print(f"Part2: {last}")
