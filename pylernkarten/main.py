import traceback
import sys
import shlex
import os

class Card:
    def __init__(self,text,back):
        self.text = text
        self.back = back

    def matches(self,guess):
        return self.back == guess or guess in self.back

    def __str__(self):
        return self.text + "|" + self.back

    __repr__ = __str__


words = set()
tags = {'der': set(), 'die': set(), 'das': set(), 'noun': set()}
relations = {'plural': {}, 'meaning':{}}
decks = {}
commands = {}
current_deck = None

def command(func):
    commands[func.__name__] = func
    return func

def parse_command(string):
    string = string.strip()
    aslist = shlex.split(string)

    def _command():
        try:
            comm = commands[aslist[0]]
            comm(*aslist[1:])
        except Exception:
            traceback.print_exc(file=sys.stdout)

    return _command

@command
def createdeck(name):
    decks[name] = []
    setdeck(name)

@command
def setdeck(name):
    global current_deck
    current_deck = name

@command
def closedeck():
    setdeck(None)

@command
def add(word):
    global current_deck
    
    if current_deck is None:
        print("No deck is set")
        return

    if word not in words:
        print("This word does not exist.")
        return

    decks[current_deck].append(word)

@command
def showdeck(name):
    print(decks[name])

@command
def clear():
    os.system("clear")

@command
def showgender(noun):
    if noun not in tags['noun']:
        print("%s is not known or not a noun" % noun)
        return

    article = article_of(noun)
    print("The article is " + article)

@command
def showmeanings(noun):
    if not relations['meaning'][noun]:
        print('No meanings found.')
        return

    print('Meanings for ' + noun)
    
    for m in relations['meaning'][noun]:
        print('* ' + m)
    

@command
def addnoun(article, noun):
    words.add(noun)
    tags[article].add(noun)
    tags['noun'].add(noun)

@command
def addmeaning(word, *meaning):
    meanings = relations['meaning'].get(word,set())
    meanings.update(meaning)
    relations['meaning'][word] = meanings

@command
def listnouns():
    print(tags['noun'])

@command
def createalias(comm, alias):
    commands[alias] = commands[comm]

@command
def saveworkspace():
    save_workspace()

@command
def exit():
    sys.exit(0)

def article_of(noun):
    if noun in tags['die']:
        return 'die'

    if noun in tags['der']:
        return 'der'

    if noun in tags['das']:
        return 'das'
    
def save_nouns(f):
    for noun in tags['die']:
        f.write("n die " + noun + "\n")

    for noun in tags['der']:
        f.write("n der " + noun + "\n")

    for noun in tags['das']:
        f.write("n das " + noun + "\n")
        

def save_meanings(f):
    for k, v in relations['meaning'].items():
        ms = ('"%s"' % s for s in v)
        f.write('addmeaning %s %s\n' % (k,' '.join(ms)))
    
def save_workspace():
    with open("workspace.txt", "w") as f:
        save_nouns(f)
        save_meanings(f)

def load_workspace():
    try:
        with open("workspace.txt") as f:
            for line in f:
                comm = line.strip()
                parse_command(comm)()
    except:
        pass

def add_default_aliases():
    createalias("addnoun", "n")
    createalias("addmeaning", "m")
    createalias("listnouns", "ln")
    
def main_loop():
    while True:
        command = parse_command(input())
        command()


add_default_aliases()
load_workspace()
main_loop()
