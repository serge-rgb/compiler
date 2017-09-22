import re
from collections import deque

import pdb

KEYWORDS = [
    'auto', 'break', 'case', 'char', 'const', 'continue', 'default',
    'do', 'double', 'else', 'enum', 'extern', 'float', 'for', 'goto',
    'if', 'inline', 'int', 'long', 'register', 'restrict', 'return',
    'short', 'signed', 'sizeof', 'static', 'struct', 'switch', 'typedef',
    'union', 'unsigned', 'void', 'volatile', 'while', '_Bool', '_Complex',
    '_Imaginary',
]

PUNCTUATORS = [
    '[', ']', '(', ')', '{', '}', '.', '->',
    '++', '--', '&', '*', '+', '-', '~', '!',
    '/', '%', '<<', '>>', '<', '>', '<=', '>=', '==', '!=', '^', '|', '&&', '||',
    '?', ':', ';', '...',
    '=', '*=', '/=', '%=', '+=', '-=', '<<=', '>>=', '&=', '^=', '|=',
    ',', '#', '##',
    '<:', ':>', '<%', '%>', '%:', '%:%:',
]

ONE_CHAR_PUNCTUATORS = PUNCTUATORS

class CharStream:
    'A stream with lookahead'
    def __init__(self, filename):
        self._lookahead = deque()
        self._stream = self.stream_file(filename)

    def stream_file(self, filename):        
        with open(filename) as file:
            for line in file:
                for char in line:
                    yield char

    def peek(self, i=1):
        while len(self._lookahead) < i:
            # TODO: Make a test that triggers an exception on this call to 'next'
            self._lookahead.append(next(self._stream))        
        return self._lookahead[i - 1]

    def pop(self):
        if len(self._lookahead) > 0:
            return self._lookahead.popleft()
        else:
            return next(self._stream)

    def empty(self):
        try:
            self._lookahead.append(next(self._stream))
        except StopIteration:
            pass

        if (len(self._lookahead) > 0):
            return False
        else:
            return True


def enum_string(cl, i):
    'Given a class cl, return the variable name of the enum of value i'    
    for key in cl.__dict__:
        val = cl.__dict__[key]
        if isinstance(val, int):
            if val == i:
                return str(key)    

class Token:  
    KEYWORD = 1
    IDENTIFIER = 2
    CONSTANT = 3
    STRING_LITERAL = 4
    PUNCTUATOR = 5  

    def __init__(self):        
        self.str = None
        self.type = None

    def __str__(self):        
        return '[' + enum_string(Token, self.type) + ' ' + self.str + ']'

def identifyKeyword(str):
    if str in KEYWORDS:        
        return True    
    return False

def identifyPunctuator(stream):    
    if stream.peek() == '(':
        p = stream.pop()
        tok = Token()
        tok.type = Token.PUNCTUATOR
        tok.str = p
        return True, tok
    else:
        return False, None

def getToken(stream):        
    tok = None
    while stream.peek().isspace():
        stream.pop()        
    if stream.peek().isalpha():  
        tok = Token()
        token_str = ''
        while stream.peek().isalnum():
            token_str += stream.pop()        
        tok.str = token_str
        if identifyKeyword(token_str):
            tok.type = Token.KEYWORD
        else:
            tok.type = Token.IDENTIFIER
    else:
        is_punct, tok = identifyPunctuator(stream)
        if not is_punct:
            tok = None
    return tok

stream = CharStream('test.c')

def main():
    for t in getToken(stream):
        print(t)

token = getToken(stream)
while token:
    print(token)
    token = getToken(stream)
    
