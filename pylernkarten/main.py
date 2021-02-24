import sys
import pylernkarten.commands as commands
from pylernkarten.commands import createalias, main_loop
import pylernkarten.words
import pylernkarten.play
import pylernkarten.decks
import pylernkarten.feeds
import pylernkarten.workspace as workspace
import pylernkarten.flashcards
import pylernkarten.dictionary

def add_default_aliases():
    createalias("addnoun", "n")
    createalias("addmeaning", "m")
    createalias("listnouns", "ln")
    createalias("saveworkspace", "sw")
    createalias("diederdas", "ddd")
    createalias("addplural", "pl")
    createalias("translate", "$")

def main():
    commands.on_reload = lambda m: workspace.load_workspace()
    add_default_aliases()
    workspace.load_workspace()
    input_str = ""
    output = ""
    
    if len(sys.argv) > 2:
        input_str = sys.argv[2]

    if len(sys.argv) > 3:
        output = sys.argv[3]
    
    main_loop(input_str, output)

if __name__ == "__main__":
    main()
