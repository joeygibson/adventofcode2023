#!/usr/bin/env python3
import queue
from dataclasses import dataclass
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
            results.append(2 ** (card[1] - 1))

    return sum(results)


@dataclass
class Card:
    name: str
    winning_numbers: list[str]
    my_numbers: list[str]

    def get_matches(self) -> set[str]:
        return set(self.winning_numbers).intersection(set(self.my_numbers))

    def get_card_number(self) -> int:
        return int(self.name.removeprefix('Card').strip())


def part2(lines: list[str]) -> int:
    cards: dict[str, Card] = {}

    for line in lines:
        chunks = line.split(': ')
        raw_winning_numbers, raw_my_numbers = chunks[1].split(' | ')
        winning_numbers = raw_winning_numbers.split()
        my_numbers = raw_my_numbers.split()

        name_chunks = chunks[0].split()
        card_name = f'Card {name_chunks[1]}'
        cards[card_name] = Card(card_name, winning_numbers, my_numbers)

    total_cards: int = 0

    card_stash = queue.Queue()
    for card in cards.values():
        card_stash.put(card)

    while not card_stash.empty():
        card = card_stash.get()
        total_cards += 1

        matches = card.get_matches()

        for i in range(1, len(matches) + 1):
            card_number = card.get_card_number()
            new_card_number = f'Card {card_number + i}'
            new_card = cards.get(new_card_number)
            if new_card:
                card_stash.put(new_card)

    return total_cards


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
