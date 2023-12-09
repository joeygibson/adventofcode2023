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


def find_node(node: Node, directions, is_part_2: bool = False) -> int:
    steps = 0

    while True:
        direction = next(directions)

        if is_part_2:
            if node.name.endswith('Z'):
                return steps
        else:
            if node.name == 'ZZZ':
                return steps

        steps += 1

        if direction == 'L':
            node = node.left
        elif direction == 'R':
            node = node.right
        else:
            raise Exception(f'Invalid direction {direction}')


def build_nodes(lines: list[str]) -> Tuple[dict[str, Node], list[str]]:
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
    return find_node(nodes['AAA'], itertools.cycle(list(lines[0])))


def part2(lines: list[str]) -> int:
    nodes, dirs = build_nodes(lines)

    ending_in_a = [node for node in nodes.values() if node.name.endswith('A')]

    finishes = [find_node(node, itertools.cycle(list(lines[0])), True) for node in ending_in_a]

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

        self.assertEqual(8906539031197, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
