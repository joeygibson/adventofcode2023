#!/usr/bin/env python3

import sys
import unittest
from typing import Tuple


def build_map(lines: list[str]) -> dict[tuple[int, int], Tuple[str, int]]:
    mapping = {}

    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            mapping[(x, y)] = (c, -1)

    return mapping


def get_neighbors(mapping: dict[tuple[int, int], Tuple[str, int]], cell: Tuple[int, int]) -> list[tuple[int, int]]:
    neighbors = []

    x, y = cell

    n_coords = []

    cell_shape = mapping[(x, y)][0]

    if cell_shape == '|':
        n_coords.append((x, y - 1))
        n_coords.append((x, y + 1))
    elif cell_shape == '-':
        n_coords.append((x - 1, y))
        n_coords.append((x + 1, y))
    elif cell_shape == 'L':
        n_coords.append((x + 1, y))
        n_coords.append((x, y - 1))
    elif cell_shape == 'J':
        n_coords.append((x - 1, y))
        n_coords.append((x, y - 1))
    elif cell_shape == '7':
        n_coords.append((x - 1, y))
        n_coords.append((x, y + 1))
    elif cell_shape == 'F':
        n_coords.append((x + 1, y))
        n_coords.append((x, y + 1))
    elif cell_shape == 'S':
        for n_coord in [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]:
            n_neighbors = get_neighbors(mapping, n_coord)
            if cell in n_neighbors:
                n_coords.append(n_coord)

    for n_coord in n_coords:
        if n_coord in mapping:
            neighbors.append(n_coord)

    return neighbors


def find_ends(mapping: dict[tuple[int, int], Tuple[str, int]]) -> list[tuple[int, int]]:
    start = [k for k, v in mapping.items() if v[0] == 'S'][0]

    return get_neighbors(mapping, start)


def walk_pipes(mapping: dict[tuple[int, int], Tuple[str, int]], cell: Tuple[int, int]):
    starting_cell = [k for k, v in mapping.items() if v[0] == 'S'][0]
    steps = 1
    done = False

    prev_cells = [starting_cell]

    while not done:
        this_cells_neighbors = get_neighbors(mapping, cell)
        if starting_cell in this_cells_neighbors and steps > 1:
            steps += 1
            done = True
            break

        next_cell = [n for n in this_cells_neighbors if n != prev_cells[-1]][0]
        prev_cells.append(cell)
        cell = next_cell

        steps += 1

    return steps / 2


def part1(lines: list[str]) -> int:
    mapping = build_map(lines)
    ends = find_ends(mapping)

    results = []

    for end in ends:
        results.append(walk_pipes(mapping, end))

    return max(results)


def part2(lines: list[str]) -> int:
    mapping = build_map(lines)
    non_pipes = [k for k, v in mapping.items() if v[0] == '.']

    for cell in non_pipes:
        print(cell)


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0a.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)

        self.assertEqual(4, res)

    def test_part2(self):
        with open('input3.txt') as f:
            self.lines = f.read().splitlines()

        res = part2(self.lines)
        self.assertEqual(4, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
