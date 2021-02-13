import feedparser
from pylernkarten.commands import command
from pylernkarten.dictionary import *

slowgerman_items = []

@command
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
    

@command
def show_words(number, filter = ""):
    global slowgerman_items
    item = slowgerman_items[int(number)]
    summary = extract_words(item["summary"])

    words = {
        word.strip(): fd_de(word.strip()) for word in summary.split()
        if validate_word(word.strip())
    }

    return (
        f"{word}: {str(meaning)}"
        for word, meaning in words.items()
        if meaning and filter.lower() in word.lower()
    )

    
