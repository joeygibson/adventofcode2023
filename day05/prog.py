#!/usr/bin/env python3
import itertools
import sys
import unittest


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


def process_seed_range(seed_range: range, maps: dict[str, ThingToThingMap]) -> list[int]:
    locations: list[int] = []

    for seed in seed_range:
        soil = maps['seed-to-soil'].map_input(seed)
        fertilizer = maps['soil-to-fertilizer'].map_input(soil)
        water = maps['fertilizer-to-water'].map_input(fertilizer)
        light = maps['water-to-light'].map_input(water)
        temperature = maps['light-to-temperature'].map_input(light)
        humidity = maps['temperature-to-humidity'].map_input(temperature)
        location = maps['humidity-to-location'].map_input(humidity)

        locations.append(location)

    return locations


# def remap(start: int, end: int, seeds: list[tuple[int, int]],
#           new_seeds: list[tuple[int, int]], m: list[int]):
#     for dest_range_start, src_range_start, range_len in m:
#         # check if ranges overlap
#         overlap_start = max(start, src_range_start)
#         overlap_end = min(end, src_range_start + range_len)
#
#         if overlap_start < overlap_end:
#             new_seeds.append(
#                 (
#                     dest_range_start + (overlap_start - src_range_start),
#                     dest_range_start + (overlap_end - src_range_start),
#                 )
#             )
#
#             if start < overlap_start:
#                 seeds.append((start, overlap_start))
#
#             if overlap_end < end:
#                 seeds.append((overlap_end, end))
#
#             break
#         else:
#             # if no overlap, add original range to new_seeds
#             new_seeds.append((start, end))


def part2(lines: list[str]) -> int:
    # borrowed from https://github.com/xHyroM/aoc/blob/main/2023/05/second_without_bruteforce.py
    seeds = []

    seed_seeds = [int(chunk) for chunk in lines[0].split()[1:]]

    for seed_seed in itertools.batched(seed_seeds, 2):
        seeds.append((seed_seed[0], seed_seed[0] + seed_seed[1]))

    def remap(start: int, end: int, new_seeds: list[tuple[int]], m: list[int]) -> int:
        for destination_range_start, source_range_start, range_length in m:
            # Check if the ranges overlap
            overlap_start = max(start, source_range_start)
            overlap_end = min(end, source_range_start + range_length)

            if overlap_start < overlap_end:
                new_seeds.append(
                    (
                        destination_range_start + (overlap_start - source_range_start),
                        destination_range_start + (overlap_end - source_range_start),
                    )
                )

                if start < overlap_start:
                    seeds.append((start, overlap_start))

                if overlap_end < end:
                    seeds.append((overlap_end, end))

                break
        else:
            # If no overlap, just add the original range to the new seeds
            new_seeds.append((start, end))

    sections = split_out_sections(lines[2:])

    maps = []

    for section in sections:
        maps.append([[int(chunk) for chunk in line.split()] for line in section[1:]])

    for m in maps:
        new_seeds = []
        while len(seeds) > 0:
            start, end = seeds.pop()
            remap(start, end, new_seeds, m)

        seeds = new_seeds

    return min(seeds)[0]


class TestDay5(unittest.TestCase):
    def test_part1(self):
        with open('input0.txt') as f:
            lines = f.read().splitlines()

        part1(lines)

    def test_part2(self):
        with open('input1.txt') as f:
            lines = f.read().splitlines()

        part2(lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
