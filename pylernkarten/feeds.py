import feedparser
from commands import command

_slowgerman_items = []

@command
def loadsg():
    feed = feedparser.parse("https://feeds.podcastmirror.com/slowgerman")
    _slowgerman_items = list(feed["items"])

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

@command
def sg2deck(number):
    item = slowgerman_items[int(number)]
    summary = extract_words(item["summary"])
    words = set( word.strip() for word in summary.split() )
    print(words)    
