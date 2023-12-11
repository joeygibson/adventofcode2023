#!/usr/bin/env python3
import itertools
import sys
import unittest


def build_univere(lines: list[str]) -> list[list[str]]:
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


def expand_height(universe: list[list[str]], ex_rows: list[int]) -> list[list[str]]:
    new_universe = universe.copy()

    for i in reversed(ex_rows):
        new_universe.insert(i, ['.'] * len(universe[0]))

    return new_universe


def expand_width(universe: list[list[str]], ex_cols: list[int]) -> list[list[str]]:
    new_universe = universe.copy()

    for i in reversed(ex_cols):
        for row in new_universe:
            row.insert(i, '.')

    return new_universe


def expand_universe(universe: list[list[str]], ex_rows: list[int], ex_cols: list[int]) -> list[list[str]]:
    new_universe = expand_height(universe, ex_rows)
    new_universe = expand_width(new_universe, ex_cols)

    return new_universe


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


def part1(lines: list[str]) -> int:
    universe = build_univere(lines)
    for row in universe:
        print(row)

    ex_rows = find_expandable_rows(universe)
    ex_cols = find_expandable_columns(universe)

    print(f'expandable rows: {ex_rows}, expandable columns: {ex_cols}')

    new_universe = expand_universe(universe, ex_rows, ex_cols)
    for row in new_universe:
        print(row)

    galaxies = find_galaxies(new_universe)
    # numbered_galaxies = list(zip(range(1, len(galaxies) + 1), galaxies))

    pairs = list(itertools.combinations(galaxies, 2))

    print(f'galaxies: {galaxies}')
    # print(f'numbered galaxies: {numbered_galaxies}')
    print(f'pairs: {pairs}, len: {len(pairs)}')

    distances = compute_distances(pairs)
    print(f'distances: {distances}')

    return sum(distances)


def part2(lines: list[str]) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)

        self.assertEqual(374, res)

    def test_part2(self):
        res = part2(self.lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
