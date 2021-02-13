from pylernkarten.commands import command
from random import shuffle

_words = set()
_tags = {'der': set(), 'die': set(), 'das': set(), 'noun': set()}
_relations = {'plural': {}, 'meaning':{}}

def plurals():
    return _relations['plural'].items()

def masculine():
    return _tags['der']

def feminine():
    return _tags['der']

def neutral():
    return _tags['der']

def meanings():
    return _relations['meaning'].items()

def meaning_of(word):
    return list(_relations['meaning'][word])

@command
def addplural(noun, plural):
    _relations['plural'][noun] = plural

def plural_of(noun):
    return _relations['plural'].get(noun, "")
    
@command
def showplural(noun):
    print(plural_of(noun))

@command
def gender(noun):
    if noun not in _tags['noun']:
        print("%s is not known or not a noun" % noun)
        return

    article = article_of(noun)
    print("The article is " + article)

@command
def meaning(noun):
    if not _relations['meaning'][noun]:
        print('No meanings found.')
        return

    print('Meanings for ' + noun)
    
    for m in _relations['meaning'][noun]:
        print('* ' + m)    

@command
def addnoun(article, noun):
    _words.add(noun)
    _tags[article].add(noun)
    _tags['noun'].add(noun)

@command
def addmeaning(word, *meaning):
    meanings = _relations['meaning'].get(word,set())
    meanings.update(meaning)
    _relations['meaning'][word] = meanings

@command
def listnouns():
    print(_tags['noun'])

@command
def saveworkspace():
    save_workspace()

def article_of(noun):
    if noun in _tags['die']:
        return 'die'

    if noun in _tags['der']:
        return 'der'

    if noun in _tags['das']:
        return 'das'

    return None

