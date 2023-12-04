#!/usr/bin/env python3

import sys


def part1(lines: list[str]) -> int:
    winning_cards = []

    for line in lines:
        chunks = line.split(': ')
        raw_winning_numbers, raw_my_numbers = chunks[1].split(' | ')
        winning_numbers = raw_winning_numbers.split()
        my_numbers = raw_my_numbers.split()

        matches = set(winning_numbers).intersection(set(my_numbers))

        if matches:
            winning_cards.append((chunks[0], len(matches)))

    results = []
    for card in winning_cards:
        if card[1] == 1:
            results.append(1)
        else:
            results.append(2**(card[1]-1))

    return sum(results)



def part2(lines: list[str]) -> int:
    pass


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
