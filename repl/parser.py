'''
A command parser written using ply

Kris Micinski
Yihao Sun
'''


tokens = (
    'ID','NUMBER', 'LPAREN','RPAREN','STRING'
    )
# Tokens

t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_STRING  = r'".*?"'
t_ID    = r'[a-zA-Z_][a-zA-Z0-9_]*'

def t_NUMBER(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)
    
# Build the lexer
import ply.lex as lex
lexer = lex.lex()

# Parsing rules

precedence = (
    # ('left','PLUS','MINUS'),
    # ('left','TIMES','DIVIDE'),
    # ('right','UMINUS'),
)

from repl.commands import *

s = None

CMD = ['help', 'run', 'connect', 'dump', 'showdb', 'csv', 'edb', 'idb', 'load', 'commit', 'refresh']

def p_statement_unary(t):
    'statement : ID'
    unary_cmd = t[1].strip()
    if unary_cmd == 'help':
        t[0] = HelpCommand()
    elif unary_cmd == 'edb':
        t[0] = EdbCommand()
    elif unary_cmd == 'edb':
        t[0] = IdbCommand()
    elif unary_cmd == 'showdb':
        t[0] = NotImplCommand(unary_cmd)
    elif unary_cmd == 'refresh':
        t[0] = RefreshCommand()
    elif unary_cmd == 'commit':
        t[0] = NotImplCommand(unary_cmd)
    else:
        print(f"Unrecognized unary command {unary_cmd} syntax, please type `help`!")

def p_statement_id_cmd(t):
    'statement : ID ID'
    id_cmd = t[1]
    if id_cmd == "dump":
        t[0] = IdCommand(t[2])
    else:
        print("Unrecognized ID, please type `help`!")

def p_statement_str_cmd(t):
    'statement : ID STRING'
    str = t[2][1:-1]
    if t[1] == "run":
        t[0] = RunCommand(str)
    elif t[1] == "connect":
        t[0] = ConnectCommand(str)
    elif t[1] == "csv":
        t[0] = CsvCommand(str)
    elif t[1] == "load":
        t[0] = LoadCommand(str)
    else:
        print("Unrecognized str command syntax, please type `help`!")

def p_error(t):
    if t:
        print("Unrecognized command at '%s'" % t.value)
    else:
        print("Unrecognized command syntax")

import ply.yacc as yacc
parser = yacc.yacc(debug=True,write_tables=True)

class CommandParser():
    def __init__(self):
        pass
    def parse(self,str):
        return parser.parse(str)

