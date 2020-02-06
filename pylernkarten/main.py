import traceback
import sys
import shlex
import os
from random import shuffle
from http.client import HTTPSConnection
import re
import urllib.request
import subprocess
import feedparser
from subprocess import PIPE
import traceback

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


words = set()
tags = {'der': set(), 'die': set(), 'das': set(), 'noun': set()}
relations = {'plural': {}, 'meaning':{}}
decks = {}
commands = {}
current_deck = None
player_command = "mpv"
slowgerman_items = []

def command(func):
    commands[func.__name__] = func
    return func

def parse_command(string):
    string = string.strip()

    if not string:
        return lambda: None
    
    aslist = shlex.split(string)

    def _escape(w):
        return w.replace("_ss_", "ß")
    
    def _command():
        try:
            comm = commands[aslist[0]]
            args = [ _escape(w) for w in aslist[1:] ]
            comm(*args)
        except Exception:
            traceback.print_exc(file=sys.stdout)

    return _command

@command
def addplural(noun, plural):
    if article_of(noun) is None:
        print("Word does not exist or not a noun.")
        return

    relations['plural'][noun] = plural

def plural_of(noun):
    return relations['plural'].get(noun, "")
    
@command
def showplural(noun):
    print(plural_of(noun))

@command
def pluraldeck(deck):
    for w in decks[deck]:
        print(w + " - " + plural_of(w))

@command
def playdeck(deck):

    for word in decks[deck]:
        print(word)
        play(word)

    
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
def remove(word):
    global current_deck
    
    if current_deck is None:
        print("No deck is set")
        return

    if word not in words:
        print("This word does not exist.")
        return

    decks[current_deck].remove(word)

@command
def showdeck(name):
    print(decks[name])

@command
def clear():
    os.system("clear")

@command
def gender(noun):
    if noun not in tags['noun']:
        print("%s is not known or not a noun" % noun)
        return

    article = article_of(noun)
    print("The article is " + article)

@command
def meaning(noun):
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
def showdecks():
    print(list(decks.keys()))

@command
def createalias(comm, alias):
    commands[alias] = commands[comm]

@command
def saveworkspace():
    save_workspace()

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
        Card(list(relations['meaning'][word])[0], word) for
        word in decks[name]
    )

@command
def playreverse(name):
    play_cards(
        Card(word, relations['meaning'][word]) for word in decks[name]
    )
    
@command 
def diederdas(name):
    play_cards(
        Card(noun, article_of(noun)) for noun in decks[name]
    )

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

    return None
    
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

def save_plurals(f):
    for k, v in relations['plural'].items():
        f.write('pl %s %s\n' % (k, v))

def save_decks(f):
    for k, v in decks.items():
        save_deck(f, k, v) 

def save_deck(f, name, deck):
    f.write("createdeck " + name + "\n")
    for word in deck:
        f.write("add " + word + "\n")
    f.write("closedeck\n")


    
def save_workspace():
    with open("workspace.txt", "w") as f:
        save_nouns(f)
        save_meanings(f)
        save_decks(f)
        save_plurals(f)

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
    createalias("saveworkspace", "sw")
    createalias("diederdas", "ddd")
    createalias("addplural", "pl")    
    
def main_loop():
    while True:
        command = parse_command(input("> "))
        command()

def file_name(word):
    return "./audio/De-" + word + ".ogg"

def save_audio(word):
    page = download_page(word)
    link, path = find_download_link(page)
    urllib.request.urlretrieve(link,filename = path)

def play_audio(word):
    word = sanitize_word(word)    
    filename = file_name(word)
    comm = subprocess.run([player_command, filename], stdout=PIPE, stderr=PIPE)#capture_output = True)

    try:
        comm.check_returncode()
    except:
        return False

    return True

@command
def play(word):
    if not os.path.isfile(word):
        try:
            save_audio(word)
        except:
            return False

    return play_audio(word)
    
@command
def listslowgerman():
    global slowgerman_items
    index = 1
    for item in slowgerman_items:
        print(str(index) + " - " + item["title"])
        if index % 10 == 0:
            if input().strip() == 'q':
                break
        index = index + 1

def extract_words(text):
    text = text.replace("\n\n", "\n").replace("?", " ").replace(",", " ").replace(".", " ").replace(":", " ").replace("/", " ")
    text = text.replace("-", " ").replace("<", " ").replace(">", " ").replace("(", " ").replace(")", " ").replace('"', " ")
    text = text.replace("!", " ").replace("-", " ").replace("_", " ").replace("%", " ").replace("$", " ").replace("#", " ")

    return text

@command
def showcommands():
    print(list(commands.keys()))

@command
def sg2deck(number):
    item = slowgerman_items[int(number)]
    summary = extract_words(item["summary"])
    words = set( word.strip() for word in summary.split() )
    print(words)
    
def find_download_link(page):
    regex = "(upload\.wikimedia\.org\/wikipedia\/commons\/[^\/]+\/[^\/]+\/[^\/]+\.ogg)"
    link = re.search(regex,page).group(1).split()[0][:-1]
    file_name = link.split("/")[-1]
    return ("http://" + link, "./audio/" +file_name)

def sanitize_word(word):
    return word.replace("ü", "%C3%BC").replace("ä", "%C3%A4").replace("ü", "%C3%9C").replace("ß", "%C3%9F")

def download_page(word):
    word = sanitize_word(word)
    base = "de.wiktionary.org"
    resource = "/wiki/" + word
    con = HTTPSConnection(base)
    con.request("GET", resource)
    resp = con.getresponse()

    return str(resp.read())

feed = feedparser.parse("https://feeds.podcastmirror.com/slowgerman")
slowgerman_items = list(feed["items"])

add_default_aliases()
load_workspace()
main_loop()
