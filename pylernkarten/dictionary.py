from pylernkarten.commands import command
import subprocess
from subprocess import PIPE
import re
import sys

if not sys.warnoptions:
    import warnings
    warnings.simplefilter("ignore")

_dictcli_de_en = [ "dict", "-d", "german-english" ]
_dictcli_en_de = [ "dict", "-d", "english-german" ]
_free_dict_eng_deu = [ "dict", "-d", "fd-eng-deu" ]
_free_dict_deu_eng = [ "dict", "-d", "fd-deu-eng" ]
_dictcli_de_pt = [ 'dict', '-d', 'fd-deu-por' ]

active_dictionary = _dictcli_de_pt

class Die:
    pass
class Der:
    pass
class Das:
    pass

genders = {
    "{m}": "Der",
    "{f}": "Die",
    "{n}": "Das"
}

def summary(word):
    output = de_en(word)
    output = output if output else []
    gender = output[0] if output else None
    meaning = fd_de(word)
    meaning = meaning if meaning else output[1:]
    if gender and meaning:
        return (word, gender, meaning)
    else:
        return None

@command
def translate(word):
    return free_dict(word, active_dictionary)

@command
def parseline():
    left, right = line.split("::")
    left = [s.strip() for s in left.split("|")]
    right = [s.strip() for s in right.split("|")]

def call_dict(word, command):
    comm = subprocess.run([ *command, word], stdout=PIPE, stderr=PIPE)
    try:
        comm.check_returncode()
        return comm.stdout.decode("utf-8")
    except:
        return False
    return True

_regex_gender_singular = re.compile('^[{]([f|m|n]*)[}]([ ][[][^]]*[]])*$')
_regex_gender_plural = re.compile('^[{]([p|l]*)[}]([ ][[][^]]*[]])*$')

_regex_par = re.compile('(^[^(]*)[(].[^)]*[)](.*)$')
_regex_bra = re.compile('(^[^[]*)[[].[^]]*[]](.*)$')
_regex_ang = re.compile('(^[^<]*)[<].[^>]*[>](.*)$')

_de_en_sep = "From German - English Dictionary 1.8.1 [german-english]:"
_en_de_sep = "From English - German Dictionary 1.8.1 [english-german]:"

def sanitize(str_):
    return remove_text(remove_text(str_, _regex_par), _regex_bra)
    
def remove_text(str_, _regex):
    m = _regex.match(str_)
    return str_ if m is None else m.group(1) + remove_text(m.group(2), _regex)

def process_definition(definition, filter_):
    word, gender_line, meaning = (i.strip() for i in list(definition.split("\n"))[0:3] if definition)
    gender_line = sanitize(gender_line).strip()

    if filter_.match(gender_line):
        return (
            word,
            gender_line,
            [i.strip() for i in sanitize(meaning).split(";")]
        )

@command
def de_en(word):
    return load_word(word, _dictcli_de_en, _de_en_sep, _regex_gender_singular)

@command
def en_de(word):
    return load_word(word, _dictcli_en_de, _en_de_sep, re.compile(".*"))
    
def load_word(word, command, separator, filter_):
    output = call_dict(word, command)
    definitions = [
        str_.strip() for str_ in
        output.split(separator)
    ] if output else []

    definitions = (process_definition(str_, filter_) for str_ in definitions[1:])
    definitions = [def_ for def_ in definitions if def_ is not None]

    if not definitions:
        return None

    gender = definitions[0][1]
    gender = genders[gender]
    
    definitions = set(def_ for defs in definitions  for def_ in defs[2] if def_)

    return (gender, definitions)

@command
def fd_eng(word):
    return free_dict(word, _free_dict_eng_deu)
    
@command
def fd_de(word):
    return free_dict(word, _free_dict_deu_eng)

def free_dict(word, command):
    try:
        output = [
            line.strip() for line in
            (line for line in call_dict(word, command).split("\n"))
        if line.strip()
        ]
    except:
        return None
    
    line = remove_text(output[-1], _regex_ang)

    return [i.strip() for i in line.split(",")]

@command
def find_plural(word):
    import pattern.en
    
    translation = fd_de(word)

    if not translation:
        return None
    
    plural = [pattern.en.pluralize(w) for w in translation]
    result = [fd_eng(p) for p in plural]

    if result and result[0]:
        return result[0][0]
