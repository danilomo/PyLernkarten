import commands
from commands import createalias, main_loop

import words
import play
import decks
import feeds
import workspace
import flashcards
import dictionary

def add_default_aliases():
    createalias("addnoun", "n")
    createalias("addmeaning", "m")
    createalias("listnouns", "ln")
    createalias("saveworkspace", "sw")
    createalias("diederdas", "ddd")
    createalias("addplural", "pl")    
    

add_default_aliases()
workspace.load_workspace()

commands.on_reload = lambda m: workspace.load_workspace()

main_loop()

