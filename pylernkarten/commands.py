import sys
import traceback
import shlex
import inspect
import importlib

_commands = {}
_modules = {}
_module_functions = {}

def on_reload(module):
    pass

def modules():
    return list(_modules.keys())

def command(func):
    _commands[func.__name__] = func
    module = inspect.getmodule(func)
    _modules[module.__name__] = module

    funcs = _module_functions.get(module.__name__, [])
    funcs.append(func.__name__)
    _module_functions[module.__name__] = funcs    
    
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
            result = comm(*args)
            
            if result:
                print(f"Output: {result}")
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

@command
def exit():
    sys.exit(0)

@command
def clear():
    os.system("clear")
    
@command
def showmodules():
    print(modules())

@command
def unload(module):
    for func in _module_functions.get(module, []):
        del _commands[func]
        
    del _module_functions[module]
    
@command
def reload(module):
    unload(module)
    importlib.reload(_modules[module])
    on_reload(module)
    
def main_loop():
    while True:
        try:
            command = parse_command(input("> "))
            command()
        except KeyboardInterrupt:
            print()
            pass
        except EOFError:
            return
