#!/usr/bin/env python3

import itertools
import sys
import unittest


def build_universe(lines: list[str]) -> list[list[str]]:
    universe = []

    for line in lines:
        universe.append(list(line))

    return universe


def find_expandable_rows(universe: list[list[str]]) -> list[int]:
    expandable_rows = []

    for i, row in enumerate(universe):
        if all([c == '.' for c in row]):
            expandable_rows.append(i)

    return expandable_rows


def find_expandable_columns(universe: list[list[str]]) -> list[int]:
    expandable_columns = []

    for i in range(0, len(universe[0])):
        if all([row[i] == '.' for row in universe]):
            expandable_columns.append(i)

    return expandable_columns


def find_galaxies(universe: list[list[str]]) -> list[tuple[int, int]]:
    galaxies = []

    for i, row in enumerate(universe):
        for j, col in enumerate(row):
            if col == '#':
                galaxies.append((i, j))

    return galaxies


def compute_distances(pairs: list[tuple[int, int]]) -> list[int]:
    distances = []

    for pair in pairs:
        dist = abs(pair[0][0] - pair[1][0]) + abs(pair[0][1] - pair[1][1])
        distances.append(dist)

    return distances


def find_galaxies_as_dict(universe: list[list[str]]) -> dict[tuple[int, int], tuple[int, int]]:
    galaxies = {}

    for i, row in enumerate(universe):
        for j, col in enumerate(row):
            if col == '#':
                galaxies[(i, j)] = (i, j)

    return galaxies


def expand_universe(galaxies: dict[tuple[int, int], tuple[int, int]],
                    ex_rows: list[int],
                    ex_cols: list[int],
                    is_part_2: bool = False) -> dict[tuple[int, int], tuple[int, int]]:
    scale = 999_999 if is_part_2 else 1

    for i in ex_rows:
        higher_galaxies = {k: v for k, v in galaxies.items() if k[0] > i}
        for k, v in higher_galaxies.items():
            galaxies[k] = (v[0] + scale, v[1])

    for i in ex_cols:
        higher_galaxies = {k: v for k, v in galaxies.items() if k[1] > i}
        for k, v in higher_galaxies.items():
            galaxies[k] = (v[0], v[1] + scale)

    return galaxies


def do_it(lines: list[str], is_part_2: bool = False) -> int:
    universe = build_universe(lines)

    ex_rows = find_expandable_rows(universe)
    ex_cols = find_expandable_columns(universe)

    galaxies = find_galaxies_as_dict(universe)

    galaxies = expand_universe(galaxies, ex_rows, ex_cols, is_part_2)

    pairs = list(itertools.combinations(galaxies.values(), 2))

    distances = compute_distances(pairs)

    return sum(distances)


def part1(lines: list[str]) -> int:
    return do_it(lines)


def part2(lines: list[str]) -> int:
    return do_it(lines, True)


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)

        self.assertEqual(374, res)

    def test_part2(self):
        res = part2(self.lines)

        self.assertEqual(82000210, res)
        # self.assertEqual(8410, res)
        # self.assertEqual(1030, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
