import sys
import words
import decks
from commands import command, parse_command

def workspace():
    if sys.argv[1:]:
        return sys.argv[1]
    else:
        return "workspace.txt"

def save_nouns(f):
    for noun in words.feminine():
        f.write("n die " + noun + "\n")

    for noun in words.masculine():
        f.write("n der " + noun + "\n")

    for noun in words.neutral():
        f.write("n das " + noun + "\n")
        

def save_meanings(f):
    for k, v in words.meanings():
        ms = ('"%s"' % s for s in v)
        f.write('addmeaning %s %s\n' % (k,' '.join(ms)))

def save_plurals(f):
    for k, v in words.plurals():
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
    with open( workspace(), "w") as f:
        save_nouns(f)
        save_meanings(f)
        save_decks(f)
        save_plurals(f)

def load_workspace():    
    try:
        with open(workspace()) as f:
            for line in f:
                comm = line.strip()
                parse_command(comm)()
    except:
        pass

@command
def saveworkspace():
    save_workspace()
