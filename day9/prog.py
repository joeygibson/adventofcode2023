#!/usr/bin/env python3
import itertools
import sys
import unittest


def compute_differences(values: list[int]) -> list[int]:
    pairs = itertools.pairwise(values)
    return [b - a for a, b in pairs]


def solve(lines: list[str], in_reverse: bool = False) -> int:
    results = []

    for line in lines:
        sequences: list[list[int]] = []

        values = [int(x) for x in line.split()]

        if in_reverse:
            values = list(reversed(values))

        sequences.append(values)

        while True:
            differences = compute_differences(values)
            sequences.append(differences)

            if all(d == 0 for d in differences):
                break

            values = differences

        rev_sequences = list(reversed(sequences))

        for i, sequence in enumerate(rev_sequences):
            if i != 0:
                sequence.append(sequence[-1] + rev_sequences[i - 1][-1])
            else:
                sequence.append(sequence[-1])

        results.append(sequences[0][-1])

    return sum(results)


def part1(lines: list[str]) -> int:
    return solve(lines)


def part2(lines: list[str]) -> int:
    return solve(lines, True)


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)

        self.assertEqual(114, res)

    def test_part2(self):
        res = part2(self.lines)
        self.assertEqual(2, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
