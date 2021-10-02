'''
A command parser written using ply

Kris Micinski
Yihao Sun
'''

import ply.lex as lex
import ply.yacc as yacc

from repl.commands import CompileCommand, RunWithDbCommand, IdCommand, ShowDbCommand, \
                          HelpCommand, NotImplCommand, ConnectCommand, LoadCommand, \
                          TagCommand

tokens = (
     'ID', 'NUMBER', 'LPAREN','RPAREN','STRING', 'HASH'
    )
# Tokens

DIGIT = r'([0-9])'
CHAR = r'([a-zA-Z])'

t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_ID    = r'[a-zA-Z][a-zA-Z0-9_]*'
# hash token

# Ignored characters
t_ignore = " \t"

def t_NUMBER(token):
    r'\d+'
    token.value = int(token.value)
    return token

def t_STRING(token):
    r'".*?"'
    token.value = token.value[1:-1]
    return token

def t_HASH(token):
    r'\[[a-zA-Z0-9]+\]'
    token.value = token.value[1:-1]
    return token

def t_newline(token):
    r'\n+'
    token.lexer.lineno += token.value.count("\n")

def t_error(token):
    print("Illegal character '%s'" % token.value[0])
    token.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Parsing rules

precedence = (
    # ('left','PLUS','MINUS'),
    # ('left','TIMES','DIVIDE'),
    # ('right','UMINUS'),
)

CMD = ['help', 'run', 'connect', 'dump', 'showdb', 'load', 'commit', 'compile', 'tag']

def p_statement_unary(cmd):
    'statement : ID'
    unary_cmd = cmd[1].strip()
    if unary_cmd == 'help':
        cmd[0] = HelpCommand()
    elif unary_cmd == 'showdb':
        cmd[0] = ShowDbCommand()
    elif unary_cmd == 'commit':
        cmd[0] = NotImplCommand(unary_cmd)
    else:
        print(f"Unrecognized unary command {unary_cmd} syntax, please type `help`!")

def p_statement_id_cmd(cmd):
    'statement : ID ID'
    id_cmd = cmd[1]
    if id_cmd == "dump":
        cmd[0] = IdCommand(cmd[2])
    else:
        print("Unrecognized ID, please type `help`!")

def p_statement_str_cmd(cmd):
    'statement : ID STRING'
    str_arg = cmd[2]
    if cmd[1] == "run":
        cmd[0] = RunWithDbCommand(str_arg)
    if cmd[1] == "connect":
        cmd[0] = ConnectCommand(str_arg)
    elif cmd[1] == "load":
        cmd[0] = LoadCommand(str_arg)
    elif cmd[1] == "compile":
        cmd[0] = CompileCommand(str_arg)
    else:
        print("Unrecognized str command syntax, please type `help`!")

def p_statement_str_id_cmd(cmd):
    '''statement : ID STRING STRING
                 | ID STRING HASH'''
    if cmd[1] == "run":
        cmd[0] = RunWithDbCommand(cmd[2], cmd[3])
    elif cmd[1] == "tag":
        cmd[0] = TagCommand(cmd[2].strip(), cmd[3])

def p_statement_tag_cmd(cmd):
    '''statement : ID HASH STRING'''
    if cmd[1] == "tag":
        cmd[0] = TagCommand(cmd[2].strip(), cmd[3])

def p_statement_complex_run_cmd(cmd):
    '''statement : ID STRING STRING NUMBER
                 | ID STRING HASH NUMBER'''
    if cmd[1] == 'run':
        cmd[0] = RunWithDbCommand(cmd[2], cmd[3], cmd[4])

def p_error(cmd):
    if cmd:
        print("Unrecognized command at '%s'" % cmd.value)
    else:
        print("Unrecognized command syntax")

parser = yacc.yacc(debug=True,write_tables=True)

class CommandParser():
    """ parser entrance """

    def parse(self, cmd):
        """ parsing command """
        return parser.parse(cmd)
