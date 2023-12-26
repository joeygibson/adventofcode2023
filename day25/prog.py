#!/usr/bin/env python3
import itertools
import random
import sys
import unittest
from collections import defaultdict, deque
from random import choice
from typing import Any

import graphviz


class KargerMinCut:
    def __init__(self, lines: list[str]):
        self.graph = defaultdict(list)
        self.edges = 0
        self.vertex_count = 0

        for line in lines:
            values = line.replace(':', '').split(' ')
            vertex = values[0]
            vertex_edges = values[1:]
            self.graph[vertex] = vertex_edges
            self.edges += len(vertex_edges)
            self.vertex_count += 1

            for edge in vertex_edges:
                self.graph[edge].append(vertex)
                self.edges += 1
                self.vertex_count += 1

        self.supervertices = {}

        for key in self.graph:
            self.supervertices[key] = [key]

    def search_min_cut(self) -> int:
        minimum_cut = 0

        while len(self.graph) > 2:
            # pick a random edge
            vertex1, vertex2 = self.pick_random_edge()
            self.edges -= len(self.graph[vertex1])
            self.edges -= len(self.graph[vertex2])

            # merge the edges
            self.graph[vertex1].extend(self.graph[vertex2])

            # update every vertex that points to vertex2 to point to vertex1
            for vertex in self.graph[vertex2]:
                if vertex2 in self.graph[vertex]:
                    self.graph[vertex].remove(vertex2)

                self.graph[vertex].append(vertex1)

            # remove self loops
            self.graph[vertex1] = [x for x in self.graph[vertex1] if x != vertex1]

            # update total edges of graph
            self.edges += len(self.graph[vertex1])
            self.graph.pop(vertex2)

            # update the grouping in the graph
            if vertex2 in self.supervertices[vertex1]:
                self.supervertices[vertex1].extend(self.supervertices.pop(vertex2))

        # now we calculate the minimum cut
        for edges in self.graph.values():
            minimum_cut = len(edges)

        # finally return the minimum cut and the two supervertices
        return minimum_cut, self.supervertices

    def pick_random_edge(self) -> tuple[str, str]:
        # rand_edge = random.randint(0, self.edges - 1)
        # rand_edge_idx = random.randint(0, self.edges - 1)
        # rand_edge = list(self.graph.keys())[rand_edge_idx]
        # for vertex, vertex_edges in self.graph.items():
        #     if len(vertex_edges) < rand_edge:
        #         rand_edge -= len(vertex_edges)
        #     else:
        #         from_vertex = vertex
        #         to_vertex = vertex_edges[rand_edge - 1]
        #         return from_vertex, to_vertex

        from_vertex = choice(list(self.graph.keys()))
        to_vertex = choice(self.graph[from_vertex])

        return from_vertex, to_vertex


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
    parts = parse(lines)
    #
    # graph = KargerMinCut(lines)
    # output = graph.search_min_cut()
    # minimum_cut = output[0]
    # supervertices = output[1]
    #
    # print(supervertices)

    # graph = {'jqt': ['rhn'],
    #          'rsh': ['frs', 'pzl', 'lsr'],
    #          'xhk': ['hfx'],
    #          'cmg': ['qnr', 'nvd', 'lhk', 'bvb'],
    #          'rhn': ['xhk', 'bvb', 'hfx'],
    #          'bvb': ['xhk', 'hfx'],
    #          'pzl': ['lsr', 'hfx', 'nvd'],
    #          'qnr': ['nvd'],
    #          'ntq': ['jqt', 'hfx', 'bvb', 'xhk'],
    #          'nvd': ['lhk'],
    #          'lsr': ['lhk'],
    #          'rzs': ['qnr', 'cmg', 'lsr', 'rsh'],
    #          'frs': ['qnr', 'lhk', 'lsr']}

    min_edges = float('inf')

    for i in range(1000):

        copy = parts.copy()

        while len(copy) > 2:
            u = random.choice(list(copy.keys()))
            v = random.choice(copy[u])
            copy[u].remove(v)
            copy[v].remove(u)
            if u in copy and v in copy:
                copy[u].extend(copy[v])
                copy[v] = None

        edges = sum(len(nodes) for nodes in copy.values() if nodes)
        min_edges = min(min_edges, edges)

    print(min_edges)


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
