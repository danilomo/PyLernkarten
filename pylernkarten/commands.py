import sys
import traceback
import shlex
import inspect
import importlib
import os
import json

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

def parse_command(string, dump_json=False):
    string = string.strip()

    if not string:
        return lambda: None
    
    aslist = shlex.split(string)

    def _escape(w):
        return w.replace("_ss_", "ß")
    
    def _command():
        try:
            if not aslist or not aslist[0]:
                return

            if aslist[0] not in _commands:
                print(f"Command {aslist[0]} not found.")
                return
                
            comm = _commands[aslist[0]]
            args = [ _escape(w) for w in aslist[1:] ]
            result = comm(*args)

            if dump_json:
                dump_output_as_json(result)
                return
            
            if not result:
                return

            if type(result) == dict:
                print_dict(result)
            elif type(result) != str and is_iterable(result):
                print_iterable(result)
            else:
                print(result)
                
        except Exception:
            traceback.print_exc(file=sys.stdout)

    return _command

def dump_output_as_json(result):
    if type(result) == dict:
        print(json.dumps(result))
    elif type(result) != str and is_iterable(result):
        print(json.dumps(list(result)))
    else:
        print(json.dumps({'result': result}))

def print_dict(result):
    for k, v in result.items():
        print(f"{k}: {v}")

def print_iterable(value):
    for val in value:
        print(f" - {val}")

@command
def createalias(comm, alias):
    _commands[alias] = _commands[comm]

@command
def showcommands(module = ""):
    if module:
        return _module_functions[module]
    else:
        return _commands.keys()

@command
def showmodules():
    return _modules.keys()

@command
def exit():
    sys.exit(0)

@command
def clear():
    os.system("clear")

@command
def unload(module):
    for func in _module_functions.get(module, []):
        if func in _commands:
            del _commands[func]
    
@command
def reload(module):
    unload(module)
    try:
        importlib.reload(_modules[module])
    except Exception as e:
        print(e)
    on_reload(module)

def is_iterable(value):
    try:
        iter(value)
        return True
    except:
        return False
    
def main_loop(input_str = "> ", output = ""):
    dump_json = output == "json"
    
    while True:
        try:
            command = parse_command(
                input(input_str), dump_json
            )
            command()
        except KeyboardInterrupt: # ctrl-c
            print()
            pass
        except EOFError: # ctrl-d
            return
