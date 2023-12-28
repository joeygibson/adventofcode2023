#!/usr/bin/env python3
import sys
import unittest

import numpy as np


def mirror_match(matrix, axis: int = 0, diff: int = 0) -> int:
    if axis == 1:
        matrix = matrix.T

    for i in range(matrix.shape[0] - 1):
        first_flipped = np.flip(matrix[:i + 1], axis=0)
        second = matrix[i + 1:]

        rows = min(first_flipped.shape[0], second.shape[0])

        if np.count_nonzero(first_flipped[:rows] - second[:rows]) == diff:
            return i + 1

    return 0


def part1(sections: list[str]) -> int:
    total = 0

    for section in sections:
        data = [list(line.replace('.', '0').replace('#', '1')) for line in section.splitlines()]
        matrix = np.array(data, dtype=int)

        total += 100 * mirror_match(matrix, axis=0) + mirror_match(matrix, axis=1)

    return total


def part2(sections: list[str]) -> int:
    total = 0

    for section in sections:
        data = [list(line.replace('.', '0').replace('#', '1')) for line in section.splitlines()]
        matrix = np.array(data, dtype=int)

        total += 100 * mirror_match(matrix, axis=0, diff=1) + mirror_match(matrix, axis=1, diff=1)

    return total


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().split("\n\n")

    def test_part1(self):
        res = part1(self.lines)

        self.assertEqual(405, res)

    def test_part2(self):
        res = part2(self.lines)

        self.assertEqual(400, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().split("\n\n")

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
