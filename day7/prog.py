#!/usr/bin/env python3
import dataclasses
import sys
import unittest

FACE_CARDS: dict[str, int] = {
    'T': 10,
    'J': 11,
    'Q': 12,
    'K': 13,
    'A': 14,
}


@dataclasses.dataclass()
class Hand:
    cards: list[str]
    bid: int

    def __post_init__(self):
        self.card_counts = {}

        for card in self.cards:
            if card in self.card_counts:
                self.card_counts[card] += 1
            else:
                self.card_counts[card] = 1

    def __str__(self):
        return f'Hand({self.cards})'

    def is_five_of_a_kind(self) -> bool:
        return len(self.card_counts) == 1

    def is_four_of_a_kind(self) -> bool:
        return len(self.card_counts) == 2 and 4 in self.card_counts.values()

    def is_full_house(self) -> bool:
        return len(self.card_counts) == 2 and 3 in self.card_counts.values()

    def is_three_of_a_kind(self) -> bool:
        return len(self.card_counts) == 3 and 3 in self.card_counts.values()

    def is_two_pair(self) -> bool:
        return len(self.card_counts) == 3 and 2 in self.card_counts.values()

    def is_one_pair(self) -> bool:
        return len(self.card_counts) == 4 and 2 in self.card_counts.values()

    def is_high_card(self) -> bool:
        return len(self.card_counts) == 5 and 1 in self.card_counts.values()

    def get_strength(self) -> int:
        if self.is_five_of_a_kind():
            return 7
        elif self.is_four_of_a_kind():
            return 6
        elif self.is_full_house():
            return 5
        elif self.is_three_of_a_kind():
            return 4
        elif self.is_two_pair():
            return 3
        elif self.is_one_pair():
            return 2
        elif self.is_high_card():
            return 1
        else:
            raise Exception(f'Unknown hand: {self}')

    def compare_cards(self, other: 'Hand') -> int:
        for pos, card in enumerate(self.cards):
            other_card = other.cards[pos]
            if card in FACE_CARDS:
                card = FACE_CARDS[card]
            else:
                card = int(card)

            if other_card in FACE_CARDS:
                other_card = FACE_CARDS[other_card]
            else:
                other_card = int(other_card)

            print(f'comparing {card} to {other_card}')

            if card < other_card:
                return -1
            elif card > other_card:
                return 1

        return 0

    def __lt__(self, other):
        if self.get_strength() < other.get_strength():
            return True
        elif self.get_strength() > other.get_strength():
            return False
        else:
            print(f'comparing {self} to {other}')
            return self.compare_cards(other) < 0


def part1(lines: list[str]) -> int:
    hands = []

    for line in lines:
        raw_cards, bid = line.split()
        cards = list(raw_cards)

        hand = Hand(cards, int(bid))

        hands.append(hand)

    values: list[int] = []

    for pos, hand in enumerate(sorted(hands), start=1):
        value = hand.bid * pos
        values.append(value)

        print(f'{pos}: {hand} -> {value}')

    return sum(values)


def part2(lines: list[str]) -> int:
    pass


class TestDay7(unittest.TestCase):
    def test_part1(self):
        with open('input0.txt') as f:
            lines = f.read().splitlines()

        res = part1(lines)

        self.assertEqual(6440, res)

    def test_part2(self):
        pass


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
