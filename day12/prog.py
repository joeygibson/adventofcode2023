#!/usr/bin/env python3
import itertools
import re
import sys
import unittest


def parse(lines: list[str]) -> list[tuple[str, list[int]]]:
    data = []

    for line in lines:
        chunks = line.split(' ')
        broken_spring_counts = [int(x) for x in chunks[1].split(',')]
        data.append((chunks[0], broken_spring_counts))

    return data


def part1(lines: list[str]) -> int:
    data = parse(lines)

    total = 0

    for line in data:
        groups = [f'#{{{i}}}' for i in line[1]]
        regex = r'\.+'.join(groups)

        if line[0].startswith('.'):
            regex = r'^\.' + regex
        else:
            regex = r'^\.*' + regex

        if line[0].endswith('.'):
            regex = regex + r'\.$'
        else:
            regex = regex + r'\.*$'

        all_possibilities = list([''.join(x) for x in itertools.product(['.', '#'], repeat=len(line[0]))])

        for p in all_possibilities:
            print(p)
        possibilities = []

        for possibility in all_possibilities:
            if re.match(regex, possibility):
                possibilities.append(possibility)

        total += len(possibilities)

    return len(possibilities)


def part2(lines: list[str]) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)

        self.assertEqual(21, res)

    def test_part2(self):
        res = part2(self.lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
