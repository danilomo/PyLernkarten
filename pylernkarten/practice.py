from random import shuffle
from pylernkarten.commands import command

_questions = [
"""- Was kostet denn ___ Schrank
- ___ kostet 799 euro.
"""
]

def parse_question(text):
    text = text.split('___')
    elements = []
    for i in range(0, len(text)):
        elements.append(('text', text[i]))
        elements.append(('filler',[]))

    elements.pop()

    result = []
    for element in elements:
        elem_type, value = element
        if elem_type != 'text' or '\n' not in value:
            result.append(element)
            continue

        lines = value.split('\n')
        
        for line in lines:
            result.append(('text', line))
            result.append(('newline',))
            
        result.pop()

    return [ element for element in result if element[0] != 'text' or (element[0] == 'text' and element[1] != "")]
        

@command
def question(number):
    return parse_question(_questions[int(number)])
