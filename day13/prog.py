#!/usr/bin/env python3
import sys
import unittest

import numpy as np


def parse(lines: list[str]) -> list[list[str]]:
    return [list(l) for l in lines]


def split_out_sections(lines: list[str]) -> list[list[str]]:
    sections = []

    section = []

    for line in lines:
        if line.strip():
            section.append(line)
        else:
            sections.append(section)
            section = []

    sections.append(section)

    return sections


def fold_horizontally(matrix: list[list[str]], split_at: int) -> tuple[list[list[str]], list[list[str]]]:
    first_piece = matrix[:split_at]
    second_piece = matrix[split_at:]

    return first_piece, second_piece


def fold_vertically(matrix: list[list[str]], split_at: int) -> tuple[list[list[str]], list[list[str]]]:
    first_piece = [row[:split_at] for row in matrix]
    second_piece = [row[split_at:] for row in matrix]

    return first_piece, second_piece


def part1(lines: list[str]) -> int:
    sections = split_out_sections(lines)

    reflected_columns = 0
    reflected_rows = 0

    for section in sections:
        data = parse(section)
        matrix = np.array(data)

        # look for vertical matches
        for i in range(1, len(matrix[0])):
            first, second = np.array_split(matrix, [i], axis=1)
            first_flipped = np.fliplr(first)

            if len(first_flipped[0]) < len(second[0]):
                first_cmp = first_flipped
                second_cmp = np.delete(second, np.s_[len(first_flipped[0]):], axis=1)
            else:
                first_cmp = np.delete(first_flipped, np.s_[len(second[0]):], axis=1)
                second_cmp = second

            if np.array_equal(first_cmp, second_cmp):
                reflected_columns += len(first[0])
                break

        # look for horizontal matches
        for i in range(1, len(matrix)):
            first, second = np.array_split(matrix, [i], axis=0)
            first_flipped = np.flipud(first)

            if len(first_flipped) < len(second):
                first_cmp = first_flipped
                second_cmp = np.delete(second, np.s_[len(first_flipped):], axis=0)
            else:
                first_cmp = np.delete(first_flipped, np.s_[len(second):], axis=0)
                second_cmp = second

            if np.array_equal(first_cmp, second_cmp):
                reflected_rows += len(first)
                break

    return reflected_columns + (reflected_rows * 100)


def part2(lines: list[str]) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().strip().splitlines()

    def test_part1(self):
        res = part1(self.lines)

        self.assertEqual(405, res)

    def test_part2(self):
        res = part2(self.lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
