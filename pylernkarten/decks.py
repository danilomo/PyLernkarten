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
def showdeck(name, filter=None):
    if not filter:
        return {word: meaning_of(word) for word in _decks[name]}

    if filter == 'nouns':
        return [
            [word, article_of(word), plural_of(word), meaning_of(word)]
            for word in _decks[name]
            if is_noun(word)
        ]
            
        

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
def deletedeck(name):
    del _decks[name]
    
@command
def createdeck(name):
    _decks[name] = []
    setdeck(name)

@command
def countdeck(name):
    if name in _decks:
        return f"Deck '{name}' has {len(_decks[name])} words"
    
@command
def showdecks():
    return list(_decks.keys())

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
            if not(gender or meaning):
                continue
            
            addnoun(gender.lower(), word)
            add(word)
            addmeaning(word, str(meaning))
            
        closedeck()

def chunks(lst, n):
    for i in range(0, len(lst), n):
        yield lst[i:i + n]
        
@command
def split_deck(name, number):
    number = int(number)
    sub_decks = list(chunks(_decks[name], number))

    for i, sub_deck in zip(range(1, len(sub_decks)+1), sub_decks):
        createdeck(f"{name}_{i}")
        for word in sub_deck:
            add(word)
        closedeck()    

@command
def load_duolingo():
    createdeck("duolingo_nouns")

    with open("duolingo.txt") as file_handle:
        words = []
        for line in file_handle:
            cols = line.strip().split()
            word = cols[0]
            category = cols[1]

            if category == "Noun":
                sum = summary(word)
                if not sum:
                    continue

                _, gender, meaning = sum

                if not (gender or meaning):
                    continue
                
                addnoun(gender.lower(), word)
                add(word)
                addmeaning(word, str(meaning))            
            
    closedeck()

        
