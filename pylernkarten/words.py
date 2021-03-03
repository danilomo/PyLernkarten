from pylernkarten.commands import command
from random import shuffle

_words = set()
_tags = {'der': set(), 'die': set(), 'das': set(), 'noun': set()}
_relations = {'plural': {}, 'meaning': {}}


def plurals():
    return _relations['plural'].items()


def masculine():
    return _tags['der']


def feminine():
    return _tags['die']


def neutral():
    return _tags['das']


def meanings():
    return _relations['meaning'].items()


def meaning_of(word):
    return _relations['meaning'][word]


def is_noun(word):
    return word in _tags['noun']


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
def update_noun(noun, article, plural, meaningof):
    addnoun(article, noun)
    addplural(noun, plural)
    addmeaning(noun, meaningof)

@command
def addnoun(article, noun):
    _words.add(noun)
    _tags[article].add(noun)
    _tags['noun'].add(noun)


@command
def addmeaning(word, meaning):
    _relations['meaning'][word] = meaning

@command
def setmeaning(word, meaning):
    _relations['meaning'][word] = meaning

@command
def listnouns():
    print(_tags['noun'])


def article_of(noun):
    if noun in _tags['die']:
        return 'die'

    if noun in _tags['der']:
        return 'der'

    if noun in _tags['das']:
        return 'das'

    return ''
