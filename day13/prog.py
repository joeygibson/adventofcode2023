#!/usr/bin/env python3
import sys
import unittest


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
        matrix = parse(section)

        vertical_matches = False

        # vertical
        for i in range(len(matrix)):
            first_piece, second_piece = fold_vertically(matrix, i + 1)

            reflects = False

            for f, s in zip(first_piece, second_piece):
                if len(f) > len(s):
                    f_cmp = list(reversed(f))[:len(s)]
                    s_cmp = s
                elif len(f) < len(s):
                    f_cmp = f
                    s_cmp = s[:len(f)]
                else:
                    f_cmp = f
                    s_cmp = s

                if list(reversed(f_cmp)) != s_cmp:
                    break

                reflects = True

            results = list(map(lambda x: x[0] == x[1], zip(reversed(first_piece), second_piece)))

            if reflects:
                reflected_columns += len(first_piece[0])
                vertical_matches = True
                continue

        if vertical_matches:
            continue

        # horizontal
        for i in range(len(matrix[0]) - 1):
            first_piece, second_piece = fold_horizontally(matrix, i + 1)

            results = list(map(lambda x: x[0] == x[1], zip(reversed(first_piece), second_piece)))

            if results and all(results):
                reflected_rows += len(first_piece)
                break

    print(f'reflected_columns: {reflected_columns}')
    print(f'reflected_rows: {reflected_rows}')

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
