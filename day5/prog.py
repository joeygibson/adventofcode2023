#!/usr/bin/env python3
import unittest
from dataclasses import dataclass
import sys


class ThingToThingMap:
    def __init__(self, lines: list[str]):
        self.ranges: dict[range, range] = {}

        for line in lines:
            dest_start, src_start, length = [int(chunk) for chunk in line.split()]

            dest_range = range(dest_start, dest_start + length)
            src_range = range(src_start, src_start + length)

            self.ranges[src_range] = dest_range

    def map_input(self, input: int) -> int:
        for src_range, dest_range in self.ranges.items():
            if input in src_range:
                return input - src_range.start + dest_range.start

        return input

    def __repr__(self):
        return f'{self.ranges}'


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


def create_maps(sections: list[list[str]]) -> dict[str, ThingToThingMap]:
    maps = {}

    for section in sections:
        name = section[0].split()[0]

        maps[name] = ThingToThingMap(section[1:])

    return maps


def part1(lines: list[str]) -> int:
    seeds = [int(chunk) for chunk in lines[0].split()[1:]]
    sections = split_out_sections(lines[2:])

    maps = create_maps(sections)

    locations: list[int] = []

    for seed in seeds:
        soil = maps['seed-to-soil'].map_input(seed)
        fertilizer = maps['soil-to-fertilizer'].map_input(soil)
        water = maps['fertilizer-to-water'].map_input(fertilizer)
        light = maps['water-to-light'].map_input(water)
        temperature = maps['light-to-temperature'].map_input(light)
        humidity = maps['temperature-to-humidity'].map_input(temperature)
        location = maps['humidity-to-location'].map_input(humidity)

        print(f'seed {seed} -> soil {soil} -> fertilizer {fertilizer} -> water {water} -> light {light} -> '
              f'temperature {temperature} -> humidity {humidity} -> location {location}')

        locations.append(location)

    return min(locations)



def part2(lines: list[str]) -> int:
    pass


class TestDay5(unittest.TestCase):
    def test_part1(self):
        with open('input0.txt') as f:
            lines = f.read().splitlines()

        part1(lines)

    def test_part2(self):
        pass


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
