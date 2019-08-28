class Card:
    def __init__(self,text,back):
        self.text = text
        self.back = back

    def matches(self,guess):
        return self.back == guess

    def __str__(self):
        return self.text + "|" + self.back

    __repr__ = __str__

class Noun:                
    def __init__(self,meaning, article, noun, plural = None, tags = {}):
        self.meaning = meaning
        self.article = article
        self.noun = noun
        self.plural = plural
        self.tags = tags

    def guess_article(self):
        return Card("___" + self.noun, self.article)

    def guess_meaning(self):
        return Card(self.noun, self.meaning)

    def guess_reverse(self):
        return Card(self.meaning, self.article + " " + self.noun)

def to_dict(l):
    return {
        v[1]: v[0] for v in zip(l, range(0,len(l)))
    }
    
def noun( str_to_parse ):
    noun, back = str_to_parse.strip().split("|")
    back = to_dict([e.strip() for e in back.split(",") ])

    article = back[0]
    meaning = back[1]
    plural  = back.get(2, None)
    tags    = back.get(3, "").split(";")

    return Noun(
        noun    = noun,
        article = article,
        meaning = meaning,
        plural  = plural,
        tags    = tags
    )       

class RegularVerb:
    def __init__(self,text):
        self.text = text

    def praesens_cards(self):

        def card(pronoun, back):
            return Card(
                pronoun + " _ (" + self.text + ")",
                back
            )

        def conjugate(root,suffix):
            if root[-1] == 't' and suffix[0] != "e":
                return root + 'e' + suffix
            return root + suffix        
        
        root = self.text[:-2]
        return [
            card("ich", conjugate(root,"e")),
            card("du", conjugate(root,"st")),
            card("er", conjugate(root,"t")),
            card("sie", conjugate(root,"t")),
            card("wir", conjugate(root,"en")),
            card("ihr", conjugate(root,"t")),
            card("Sir", conjugate(root,"en")),
        ]
