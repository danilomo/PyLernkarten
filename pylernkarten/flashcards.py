from random import shuffle
from pylernkarten.commands import command
from pylernkarten.words import article_of, meaning_of
from pylernkarten.dictionary import *
import pylernkarten.decks as decks
import json

errors = []

class Card:
    def __init__(self,text,back):
        self.text = text
        self.back = back
        self.multi_answer = type(back) is set

    def matches(self,guess):
        return self.back == guess if not self.multi_answer else guess in self.back

    def __str__(self):
        return self.text + "|" + self.back

    __repr__ = __str__

def play_cards(cards_it, json_dump=False):
    cards = list(cards_it)
    global errors
    shuffle(cards)

    def input_text(card, has_more):
        if json_dump:
            return json.dumps({'card': card.text.title(), 'notes': str(meaning_of(card.text)), 'has_more': has_more}) + "\n"
        else:
            return card.text + ": "
        
    def right_answer(card):
        if json_dump:
            return json.dumps({'right_answer': True, 'message': ''})
        else:
            return "Correct!"

    def wrong_answer(card):
        if json_dump:
            return json.dumps({'right_answer': False, 'message': 'Too bad! The correct answer was: ' + str(card.back)})
        else:
            return "Too bad! The correct answer was: " + card.back
            

    errors = []
    
    for i, card in zip(range(0, len(cards)), cards):
        has_more = i != len(cards) - 1

        guess = input(input_text(card, has_more))
        if card.matches(guess):
            print(right_answer(card))
        else:
            print(wrong_answer(card))
            errors.append(card.text)

    if json_dump:
        return

    if errors:
        print("Your errors:")
        return errors
    
    print("Well done!")

@command
def save_errors(deckname):
    createdeck(deckname)
    for error in errors:
        add(error)
    closedeck()

@command
def show_errors():
    return errors

@command
def guessdeck(name):
    play_cards(
        Card(meaning_of(word)[0], word) for
        word in decks.deck(name)
    )

@command
def playreverse(name):
    play_cards(
        Card(word, meaning_of(word)) for word in decks.deck(name)
    )
    
@command 
def diederdas(name):
    play_cards(
        (Card(noun, article_of(noun)) for noun in decks.deck(name)),
        json_dump=True
    )
