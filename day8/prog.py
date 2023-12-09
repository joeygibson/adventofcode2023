#!/usr/bin/env python3
import itertools
import math
import re
import sys
import unittest
from typing import Tuple


class Node:
    def __init__(self, name: str, left: 'Node' = None, right: 'Node' = None):
        self.name = name
        self.left = left
        self.right = right

    def __repr__(self):
        return f'Node({self.name}, {self.left}, {self.right})'

    def __str__(self):
        return f'Node({self.name}, {self.left}, {self.right})'


# def find_node(starting_nodes: list[Node], directions) -> int:
#     steps = 0
#     cur_nodes: list[Node] = starting_nodes
#     finishes: list[int] = [-1] * len(cur_nodes)
#
#     while True:
#         # matches = all(node.name.endswith('Z') for node in cur_nodes)
#         #
#         # if matches:
#         #     return steps
#
#         steps += 1
#
#         if all(f > 0 for f in finishes):
#             print(f'finishes: {finishes}')
#             return math.lcm(*finishes)
#
#         direction = next(directions)
#
#         for i, node in enumerate(cur_nodes):
#             if node.name.endswith('Z'):
#                 finishes[i] = steps
#                 continue
#
#             if direction == 'L':
#                 cur_nodes[i] = node.left
#             elif direction == 'R':
#                 cur_nodes[i] = node.right
#             else:
#                 raise Exception(f'Invalid direction {direction}')

def find_node(node: Node, directions) -> int:
    steps = 0

    while True:
        direction = next(directions)

        if node.name.endswith('Z'):
            return steps

        steps += 1

        if direction == 'L':
            node = node.left
        elif direction == 'R':
            node = node.right
        else:
            raise Exception(f'Invalid direction {direction}')


def build_nodes(lines: list[str]) -> Tuple[dict[str, Node], list[int]]:
    nodes = {}

    dirs = list(lines[0])

    for line in lines[2:]:
        node_name, left_name, right_name = re.findall(r'\w+', line)

        node = nodes.get(node_name, Node(node_name))
        nodes[node_name] = node

        left_node = nodes.get(left_name, Node(left_name))
        nodes[left_name] = left_node

        right_node = nodes.get(right_name, Node(right_name))
        nodes[right_name] = right_node

        node.left = left_node
        node.right = right_node

    return nodes, dirs


def part1(lines: list[str]) -> int:
    nodes, dirs = build_nodes(lines)
    return find_node(nodes['AAA'], 'ZZZ', dirs)


def part2(lines: list[str]) -> int:
    nodes, dirs = build_nodes(lines)

    ending_in_a = [node for node in nodes.values() if node.name.endswith('A')]

    finishes = [find_node(node, itertools.cycle(list(lines[0]))) for node in ending_in_a]

    return math.lcm(*finishes)


class TestProg(unittest.TestCase):
    def test_part1(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

        res = part1(self.lines)

        self.assertEqual(2, res)

    def test_part1_two(self):
        with open('input2.txt') as f:
            self.lines = f.read().splitlines()

        res = part1(self.lines)

        self.assertEqual(6, res)

    def test_part2_one(self):
        with open('input3.txt') as f:
            self.lines = f.read().splitlines()

        res = part2(self.lines)

        self.assertEqual(6, res)

    def test_part2(self):
        with open('input1.txt') as f:
            self.lines = f.read().splitlines()

        res = part2(self.lines)

        self.assertEqual(6, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    # print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
