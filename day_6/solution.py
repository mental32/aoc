data = []
with open("input_data") as inf:
    for line in inf:
        left, right = line.strip().split(')')
        data.append((left, right))

from collections import defaultdict

graph = defaultdict(list)

for left, right in data:
    graph[left].append(right)

from typing import Tuple

def graph_(node) -> Tuple[int, int]:
    indirect = 0

    for child in graph[node]:
        indirect += graph_(child) if child in graph else 0

    return (len(graph[node]) + indirect)

print(sum([graph_(node) for node in graph]))

def trace(node):
    tree = []
    for other, others in graph.items():
        if node in others:
            tree += trace(other)
            tree.append(other)
    return tree

you = trace("YOU")[::-1]
santa = trace("SAN")[::-1]

you_ = set(you)
san_ = set(santa)

def dist(from_, to):
    f = trace(to)
    return len(f[:f.index(from_)])

dist_ = len(you) + len(santa)

for node in {"COM"} | (san_ & you_):
    sdist = len(santa[:santa.index(node)])
    ydist = len(you[:you.index(node)])
    v = sdist + ydist

    if v <= dist_:
        dist_ = v

print(dist_)
