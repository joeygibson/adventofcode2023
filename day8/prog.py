#!/usr/bin/env python3
import itertools
import re
import sys
import unittest


class Node:
    def __init__(self, name: str, left: 'Node' = None, right: 'Node' = None):
        self.name = name
        self.left = left
        self.right = right

    def __repr__(self):
        return f'Node({self.name}, {self.left}, {self.right})'

    def __str__(self):
        return f'Node({self.name}, {self.left}, {self.right})'


# def find_node(node: Node, name: str, directions) -> int:
#     if node.name == name:
#         return 0
#
#     direction = next(directions)
#     if direction == 'L':
#         return find_node(node.left, name, directions) + 1
#     elif direction == 'R':
#         return find_node(node.right, name, directions) + 1
#     else:
#         raise Exception(f'Invalid direction {direction}')

def find_node(node: Node, name: str, directions) -> int:
    steps = 0
    prev_node = None

    while True:
        if node.name == name:
            return steps

        steps += 1

        direction = next(directions)

        if direction == 'L':
            if node.left:
                prev_node = node
                node = node.left
            else:
                node = prev_node
        elif direction == 'R':
            if node.right:
                prev_node = node
                node = node.right
            else:
                node = prev_node
        else:
            raise Exception(f'Invalid direction {direction}')


def part1(lines: list[str]) -> int:
    nodes = {}

    dirs = itertools.cycle(list(lines[0]))

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

    return find_node(nodes['AAA'], 'ZZZ', dirs)


def part2(lines: list[str]) -> int:
    pass


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

    def test_part2(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

        res = part2(self.lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
