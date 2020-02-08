import urllib.request
import os
import subprocess
from subprocess import PIPE
import re
from http.client import HTTPSConnection

from commands import command

_player_command = "mplayer"

def file_name(word):
    return "./audio/De-" + word + ".ogg"

def save_audio(word):
    page = download_page(word)
    link, path = find_download_link(page)
    urllib.request.urlretrieve(link,filename = path)

def play_audio(word):
    word = sanitize_word(word)    
    filename = file_name(word)
    comm = subprocess.run([_player_command, filename], stdout=PIPE, stderr=PIPE)#capture_output = True)
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
