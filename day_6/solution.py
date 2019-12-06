from collections import defaultdict
from typing import Tuple

with open("input_data") as inf:
    data = [line.strip().split(')') for line in inf if line.strip()]

graph = defaultdict(list)

for left, right in data:
    graph[left].append(right)

def distance(node) -> Tuple[int, int]:
    indirect = 0

    for child in graph[node]:
        if child in graph:
            indirect += distance(child)

    return (len(graph[node]) + indirect)

print(sum([distance(node) for node in graph]))

def trace(node):
    tree = []
    for other, others in graph.items():
        if node in others:
            tree += trace(other)
            tree.append(other)
    return tree

you = trace("YOU")[::-1]
santa = trace("SAN")[::-1]

dist = len(you) + len(santa)

for node in {"COM"} | (set(you) & set(santa)):
    sdist = len(santa[:santa.index(node)])
    ydist = len(you[:you.index(node)])
    v = sdist + ydist

    if v <= dist:
        dist = v

print(dist)
