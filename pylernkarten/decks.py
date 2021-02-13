from pylernkarten.commands import command
from pylernkarten.dictionary import summary
from pylernkarten.words import *

_decks = {}
_current_deck = None

def deck(name):
    return _decks[name]

def items():
    return _decks.items()

@command
def showdeck(name):
    return _decks[name]

@command
def setdeck(name):
    global _current_deck
    _current_deck = name

@command
def closedeck():
    setdeck(None)

@command
def add(word):
    global _current_deck
    
    if _current_deck is None:
        print("No deck is set")
        return

    _decks[_current_deck].append(word)

@command
def remove(word):
    global _current_deck
    
    if _current_deck is None:
        print("No deck is set")
        return

    _decks[_current_deck].remove(word)
    
@command
def playdeck(deck):
    for word in _decks[deck]:
        print(word)
        play(word)

    
@command
def createdeck(name):
    _decks[name] = []
    setdeck(name)
    
@command
def showdecks():
    print(list(_decks.keys()))

@command
def pluraldeck(deck):
    for w in decks[deck]:
        print(w + " - " + plural_of(w))

@command
def load_xlsx(filename):
    from openpyxl import load_workbook

    workbook = load_workbook(
        filename=filename
    )
    sheets = workbook.worksheets
    
    for sheet in sheets:
        createdeck(sheet.title)

        words = (
            summary(row[0])
            for row in
            sheet.values
            if summary(row[0])
        )

        for word, gender, meaning in words:
            addnoun(gender.lower(), word)
            add(word)
            addmeaning(word, str(meaning))
            
        closedeck()
