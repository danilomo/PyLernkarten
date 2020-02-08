from commands import command
from words import article_of, meaning_of

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

def play_cards(cards_it):
    cards = list(cards_it)
    shuffle(cards)

    for card in cards:
        guess = input(card.text + ": " )
        if card.matches(guess):
            print("Correct!")
        else:
            print("Too bad! The correct answer was: " + card.back)
            return

    print("Well done!")

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
        Card(noun, article_of(noun)) for noun in decks.deck(name)
    )
