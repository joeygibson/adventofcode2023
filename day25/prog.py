#!/usr/bin/env python3
import itertools
import sys
import unittest
from collections import defaultdict, deque
from random import choice
from typing import Any

import graphviz


def visualize(graph: dict[str, set]) -> None:
    dot = graphviz.Digraph()

    for node in graph:
        dot.node(node)

    added_edges = set()

    for node in graph:
        for edge in graph[node]:
            if (node, edge) not in added_edges and (edge, node) not in added_edges:
                dot.edge(node, edge)
                added_edges.add((node, edge))

    dot.render('graph.png', view=True)


def parse(lines: list[str]) -> defaultdict[Any, set]:
    graph = defaultdict(set)

    for line in lines:
        parts = line.replace(':', '').split(' ')

        for part in parts[1:]:
            graph[parts[0]].add(part)
            graph[part].add(parts[0])

    return graph


def bfs(graph, node, dest) -> list[str]:
    visited = []
    queue = deque([(node, 0)])

    while queue:
        node, dist = queue.popleft()

        if node == dest:
            break

        for neighbor in graph[node]:
            if neighbor not in visited:
                visited.append(neighbor)
                queue.append((neighbor, dist + 1))

    return visited


def part1(lines: list[str]) -> int:
    parts = parse(lines)
    # visualize(parts)

    total_nodes = len(parts.keys())

    a = choice(list(parts.keys()))
    b = choice(list(parts.keys()))

    for _ in range(4):
        path = bfs(parts, a, b)
        print(f'path -> {path}')

        if path:
            for node in path:
                if a in parts[node]:
                    parts[node].remove(a)
                if node in parts[a]:
                    parts[a].remove(node)
        else:
            print(f'no path between {a} and {b}')





    # paths = {}
    # for a, b in itertools.combinations(parts.keys(), 2):
    #     path = bfs(parts, a, b)
    #     paths[(a, b)] = path
    # print(f'distances -> {distances}')
    #
    # commonalities = defaultdict(int)
    #
    # for head, path in distances.items():
    #     for node in path:
    #         normalized_name = ', '.join(sorted([head[0], node]))
    #         commonalities[normalized_name] += 1
    #
    # sorted_by_distance = dict(reversed(sorted(commonalities.items(), key=lambda x: x[1])))
    # print(f'commonalities -> {sorted_by_distance}')


def part2(lines: list[str]) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)
        self.assertEqual(54, res)

    def test_part1_5(self):
        with open('input1.txt') as f:
            self.lines = f.read().splitlines()
        res = part1(self.lines)
        self.assertEqual(54, res)

    def test_part2(self):
        res = part2(self.lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
