import sys
import traceback
import shlex
import inspect
_commands = {}
_modules = {}

def command(func):
    _commands[func.__name__] = func
    _modules[func.__name__] = inspect.getmodule(func)
    return func

def parse_command(string):
    string = string.strip()

    if not string:
        return lambda: None
    
    aslist = shlex.split(string)

    def _escape(w):
        return w.replace("_ss_", "ß")
    
    def _command():
        try:
            comm = _commands[aslist[0]]
            args = [ _escape(w) for w in aslist[1:] ]
            comm(*args)
        except Exception:
            traceback.print_exc(file=sys.stdout)

    return _command

@command
def createalias(comm, alias):
    _commands[alias] = _commands[comm]

@command
def showcommands():
    print(list(_commands.keys()))

@command
def reload():
    pass

@command
def showmodules():
    print(_modules)
