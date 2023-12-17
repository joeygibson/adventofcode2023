#!/usr/bin/env python3
import collections
import enum
import sys
import unittest
from copy import deepcopy
from dataclasses import dataclass


@dataclass
class Spot:
    shape: str
    is_energized: bool = False


class Direction(enum.Enum):
    UP = 1
    DOWN = 2
    LEFT = 3
    RIGHT = 4


@dataclass
class Beam:
    pos: tuple[int, int]
    dir: Direction

    def move(self):
        if self.dir is Direction.UP:
            self.pos = (self.pos[0], self.pos[1] - 1)
        elif self.dir is Direction.DOWN:
            self.pos = (self.pos[0], self.pos[1] + 1)
        elif self.dir is Direction.LEFT:
            self.pos = (self.pos[0] - 1, self.pos[1])
        elif self.dir is Direction.RIGHT:
            self.pos = (self.pos[0] + 1, self.pos[1])


def build_map(lines: list[str]) -> dict[tuple[int, int], Spot]:
    cave = {}
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            cave[(x, y)] = Spot(c)

    return cave


def part1(lines: list[str]) -> int:
    cave = build_map(lines)
    beam = Beam((0, 0), Direction.RIGHT)
    beams = collections.deque([beam])
    split_spots = []

    while True:
        if not beams:
            break

        print('energized spots ->', sum([1 for spot in cave.values() if spot.is_energized]))

        beam = beams.popleft()

        while beam.pos in cave:
            spot = cave[beam.pos]

            if spot.shape == '.':
                beam.move()
            elif spot.shape == '/':
                if beam.dir is Direction.UP:
                    beam.dir = Direction.RIGHT
                elif beam.dir is Direction.DOWN:
                    beam.dir = Direction.LEFT
                elif beam.dir is Direction.LEFT:
                    beam.dir = Direction.DOWN
                elif beam.dir is Direction.RIGHT:
                    beam.dir = Direction.UP
                beam.move()
            elif spot.shape == '\\':
                if beam.dir is Direction.UP:
                    beam.dir = Direction.LEFT
                elif beam.dir is Direction.DOWN:
                    beam.dir = Direction.RIGHT
                elif beam.dir is Direction.LEFT:
                    beam.dir = Direction.UP
                elif beam.dir is Direction.RIGHT:
                    beam.dir = Direction.DOWN
                beam.move()
            elif spot.shape == '-':
                if beam.dir is Direction.UP or beam.dir is Direction.DOWN:
                    beam.dir = Direction.RIGHT

                    new_beam = Beam(beam.pos, Direction.LEFT)

                    if new_beam not in split_spots:
                        split_spots.append(deepcopy(new_beam))
                        beams.append(new_beam)

                    if beam in split_spots:
                        break

                    split_spots.append(deepcopy(beam))
                beam.move()
            elif spot.shape == '|':
                if beam.dir is Direction.LEFT or beam.dir is Direction.RIGHT:
                    beam.dir = Direction.DOWN

                    new_beam = Beam(beam.pos, Direction.UP)

                    if new_beam not in split_spots:
                        split_spots.append(deepcopy(new_beam))
                        beams.append(new_beam)

                    if beam in split_spots:
                        break

                    split_spots.append(deepcopy(beam))
                beam.move()

            spot.is_energized = True

    return sum([1 for spot in cave.values() if spot.is_energized])


def part2(lines: list[str]) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)
        self.assertEqual(46, res)

    def test_part2(self):
        res = part2(self.lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
