import feedparser
from pylernkarten.commands import command
from pylernkarten.dictionary import *
from pylernkarten.words import *
from pylernkarten.decks import *
slowgerman_items = []

def loadsg():
    global slowgerman_items
    feed = feedparser.parse("https://feeds.podcastmirror.com/slowgerman")
    slowgerman_items = list(feed["items"])

@command
def listsg():
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

def validate_word(word):
    try:
        float(word)
        return False
    except:
        pass

    return True


blacklist = set([
    "ich", "es", "aus", "es", "er", "sein", "oder", "aber", "text"
])

@command
def create_deck(number, filter = ""):
    global slowgerman_items
    item = slowgerman_items[int(number)]
    text = extract_words(item["summary"])

    print(summary)    
    
    words = set(
        word.strip() for word in text.split()
        if validate_word(word.strip())
    )
    words = (summary(word)
             for word in words
             if word and word.lower() not in blacklist and word[0].isupper()
    )
    words = (word for word in words if word)
    

    createdeck(f"sg_{number}")

    for word, gender, meaning in words:
        if not(gender or meaning):
            continue
        
        addnoun(gender.lower(), word)
        add(word)
        addmeaning(word, str(meaning))
            
    closedeck()
    
try:
    loadsg()
except Exception as ex:
    print("Warning: " + str(ex))
