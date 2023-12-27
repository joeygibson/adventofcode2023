#!/usr/bin/env python3
import enum
import sys
import unittest
from typing import Tuple


class CellType(enum.Enum):
    START = 'S'
    VERTICAL = '|'
    HORIZONTAL = '-'
    NORTH_EAST = '7'
    NORTH_WEST = 'F'
    SOUTH_EAST = 'J'
    SOUTH_WEST = 'L'
    GROUND = '.'
    LOOP_GROUND = ','
    LOOP_VERTICAL = '1'
    LOOP_HORIZONTAL = '2'
    LOOP_NORTH_EAST = '3'
    LOOP_NORTH_WEST = '4'
    LOOP_SOUTH_EAST = '5'
    LOOP_SOUTH_WEST = '6'


def build_map(lines: list[str]) -> dict[tuple[int, int], tuple[str, CellType]]:
    mapping = {}

    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            mapping[(x, y)] = (c, CellType(c))

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


def walk_pipes(mapping: dict[tuple[int, int], Tuple[str, int]], cell: Tuple[int, int]) \
        -> tuple[int, list[tuple[int, int]]]:
    starting_cell = [k for k, v in mapping.items() if v[0] == 'S'][0]
    steps = 1

    prev_cells = [starting_cell]

    while True:
        this_cells_neighbors = get_neighbors(mapping, cell)
        if starting_cell in this_cells_neighbors and steps > 1:
            steps += 1
            break

        next_cell = [n for n in this_cells_neighbors if n != prev_cells[-1]][0]
        prev_cells.append(cell)
        cell = next_cell

        steps += 1

    return steps / 2, prev_cells


def find_locations_inside_loop(mapping: dict[tuple[int, int]], height: int, width: int) -> list[tuple[int, int]]:
    locations = []

    inside = False
    last_curve = CellType.GROUND

    for row in range(height):
        for col in range(width):
            cell = mapping[(col, row)]
            match value := cell[1]:
                case CellType.GROUND | CellType.VERTICAL | CellType.HORIZONTAL | CellType.NORTH_EAST | \
                     CellType.NORTH_WEST | CellType.SOUTH_EAST | CellType.SOUTH_WEST:
                    if inside:
                        locations.append((col, row))
                case CellType.LOOP_VERTICAL:
                    inside = not inside
                case CellType.LOOP_SOUTH_WEST | CellType.LOOP_NORTH_WEST:
                    last_curve = value
                case CellType.LOOP_SOUTH_EAST:
                    if last_curve == CellType.LOOP_NORTH_WEST:
                        inside = not inside
                case CellType.LOOP_NORTH_EAST:
                    if last_curve == CellType.LOOP_SOUTH_WEST:
                        inside = not inside

    return locations


def part1(lines: list[str]) -> int:
    mapping = build_map(lines)
    ends = find_ends(mapping)

    results = []

    for end in ends:
        results.append(walk_pipes(mapping, end)[0])

    return max(results)


def part2(lines: list[str]) -> int:
    # solution borrowed from https://github.com/Mathijstb/AdventOfCode/blob/master/2023/src/main/java/day10/Day10.java
    mapping = build_map(lines)

    map_height = len(lines)
    map_width = len(lines[0])

    ends = find_ends(mapping)

    results = []

    for end in ends:
        results.append(walk_pipes(mapping, end)[1])

    for cell in results[0]:
        cell_type = None

        match mapping.get(cell)[1]:
            case CellType.VERTICAL:
                cell_type = CellType.LOOP_VERTICAL
            case CellType.HORIZONTAL:
                cell_type = CellType.LOOP_HORIZONTAL
            case CellType.NORTH_EAST:
                cell_type = CellType.LOOP_NORTH_EAST
            case CellType.NORTH_WEST:
                cell_type = CellType.LOOP_NORTH_WEST
            case CellType.SOUTH_EAST:
                cell_type = CellType.LOOP_SOUTH_EAST
            case CellType.SOUTH_WEST:
                cell_type = CellType.LOOP_SOUTH_WEST

        if cell_type:
            mapping[cell] = (cell[0], cell_type)

    ground_inside_loop = find_locations_inside_loop(mapping, map_height, map_width)

    return len(ground_inside_loop)


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0a.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)

        self.assertEqual(4, res)

    def test_part2(self):
        with open('input1.txt') as f:
            self.lines = f.read().splitlines()

        res = part2(self.lines)
        self.assertEqual(4, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
