#!/usr/bin/env python3
import sys
import unittest
from collections import defaultdict
from typing import Any

import graphviz
import networkx as nx


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


def parse(lines: list[str]) -> defaultdict[Any, list]:
    graph = defaultdict(list)

    for line in lines:
        parts = line.replace(':', '').split(' ')

        for part in parts[1:]:
            graph[parts[0]].append(part)
            graph[part].append(parts[0])

    return graph


def part1(lines: list[str]) -> int:
    G = nx.Graph()

    for line in lines:
        row = line.strip().split(':')
        for target in row[1].split():
            G.add_edge(row[0].strip(), target)

    costs = []

    for edge in G.edges:
        G.remove_edge(*edge)
        costs.append((edge, nx.shortest_path_length(G, *edge)))
        G.add_edge(*edge)

    costs = sorted(costs, key=lambda x: x[1], reverse=True)

    [G.remove_edge(*costs[i][0]) for i in range(3)]

    print(f'removing {costs[0][0]}, {costs[1][0]}, {costs[2][0]}')

    return len(nx.node_connected_component(G, costs[0][0][0])) * len(nx.node_connected_component(G, costs[0][0][1]))


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
