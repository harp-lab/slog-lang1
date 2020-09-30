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

def p_statement(t):
    'statement : ID STRING'
    if t[1] == "load":
        t[0] = LoadCommand(t[2])
    elif t[1] == "connect":
        t[0] = ConnectCommand(t[2])

def p_error(t):
    print("Syntax error at '%s'" % t.value)

import ply.yacc as yacc
parser = yacc.yacc(debug=True,write_tables=True)

class CommandParser():
    def __init__(self):
        pass
    def parse(self,str):
        return parser.parse(str)

