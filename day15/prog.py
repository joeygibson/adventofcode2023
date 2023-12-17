#!/usr/bin/env python3

import sys
import unittest


def part1(data: str) -> int:
    chunks = data.split(',')

    total = 0
    for item in chunks:
        current_value = 0

        for val, ord_val in [(l, ord(l.strip())) for l in item]:
            current_value += ord_val
            current_value *= 17
            current_value = current_value % 256

        total += current_value

    return total


def part2(data: str) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.data = f.read().replace('\n', '')

    def test_part0(self):
        res = part1('HASH')
        self.assertEqual(52, res)

    def test_part1(self):

        res = part1(self.data)
        self.assertEqual(1320, res)

    def test_part2(self):
        res = part2(self.data)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        data = f.read().replace('\n', '')

    print(f'part1 -> {part1(data)}')
    print(f'part2 -> {part2(data)}')
