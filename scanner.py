'''
 * 词法分析器
 * @version 0.2
 * @author 程梓益
 * @license MIT License
 * 
 * 缺失的特性：
 * 1.不支持Unicode；
 * 2.不支持二进制、八进制、十六进制
 * 3.不支持转义
 * 4.字符串只支持双引号
'''

from enum import Enum, unique

## 枚举 Token的类型，关键字，标识，字符串，分隔符，操作符
@unique
class TokenKind(Enum):
    Keyword = 1
    Identifier = 2
    StringLiteral = 3
    IntegerLiteral = 4
    DecimalLiteral = 5
    NullLiteral = 6
    BooleanLiteral = 7
    Seperator = 8
    Operator = 9
    EOF = 10

## Token数据结构，类型，内容
class Token:
    def __init__(self, kind:TokenKind, text:str):
        self.kind: TokenKind = kind
        self.text: str = text

'''
一个字符串流
peek():预读下一个字符，不移动指针
next():读取下一个字符，移动指针
eof():判断是否到结尾
'''
class CharStream:
    def __init__(self, data:str):
        assert type(data) is str
        self.data:str = data
        self.pos:int = 0
        self.line:int = 1
        self.col:int = 0

    def peek(self) -> str:
        return self.data[self.pos] if self.pos<len(self.data) else ""

    def next(self) -> str:
        ch = self.data[self.pos]
        self.pos += 1
        if ch == "\n":
            self.line += 1
            self.col = 0
        else:
            self.col += 1
        return ch

    def eof(self) -> bool:
        return self.peek() == ''

'''
 * 词法分析器。
 * 词法分析器的接口像是一个流，词法解析是按需进行的。
 * 支持下面几个操作：
 * next(): 返回当前的Token，并移向下一个Token。
 * peek(): 预读当前的Token，但不移动当前位置。
 * peek2(): 预读第二个Token。
'''
class Scanner:
    def __init__(self, stream:CharStream):
        self.stream = stream
        self.tokens:List[Token] = []
        self.__KeyWords:Set[str] = {
        "function", "class",     "break",       "delete",    "return",    
        "case",      "do",        "if",          "switch",    "var",
        "catch",     "else",      "in",          "this",      "void",
        "continue",  "false",     "instanceof",  "throw",     "while",
        "debugger",  "finally",   "new",         "true",      "with",  
        "default",   "for",       "null",        "try",       "typeof",
        ##下面这些用于严格模式
        "implements","let",       "private",     "public",    "yield", 
        "interface", "package",   "protected",   "static" }

    def next(self) -> Token:
        t:Token or None = self.tokens.pop(0) if len(self.tokens)>0 else None
        if t is None:
            return self.__getAToken()
        else:
            return t

    def peek(self) -> Token:
        t:Token or None = self.tokens[0] if len(self.tokens)>0 else None
        if t is None:
            t = self.__getAToken()
            self.tokens.append(t)
        return t

    def peek2(self) -> Token:
        t:Token or None = self.tokens[1] if len(self.tokens)>1 else None
        while t is None:
            self.tokens.append(self.__getAToken())
            t = self.tokens[1] if len(self.tokens)>1 else None
        return t

    def __getAToken(self) -> Token:
        '''
        从字符串流中获取一个新Token
        '''
        self.__skipWhiteSpaces()
        if self.stream.eof():
            return Token(TokenKind.EOF, "")
        else:
            ch:str = self.stream.peek()
            if self.__isLetter(ch) or ch=='_':
                return self.__parseIdentifier()
            elif ch == '"':
                ##读到 " 时解析字符串
                return self.__parseStringLiteral()
            elif ch == '(' or ch == ')' or ch == '{'\
              or ch == '}' or ch == '[' or ch == ']'\
              or ch == ',' or ch == ';' or ch == '?'\
              or ch == '@' or ch == ':':
                self.stream.next()
                return Token(TokenKind.Seperator, ch)

            #解析数字字面量：
            #DecimalLiteral:IntegerLiteral '.' [0-9]*
            #  | '.'[0-9]+
            #  | IntegerLiteral
            #  ;
            #  IntegerLiteral: '0' | [1-9] [0-9]*

            elif self.__isDigit(ch):
                self.stream.next()
                ch1 = self.stream.peek()
                literal:str = ''
                if ch == '0':##暂不支持八进制、二进制、十六进制
                    if not (ch1>='1' and ch1<='9'):
                        literal = '0'
                    else:
                        print("0 cannot be followed by other digit now, at line: "+self.stream.line+" col: "+self.stream.col)
                        ##先跳过
                        self.stream.next()
                        return self.__getAToken()
                elif ch>='1' and ch<='9':
                    literal += ch
                    while self.__isDigit(ch1):
                        ch = self.stream.next()
                        literal += ch
                        ch1 = self.stream.peek()
                ##加上小数点
                if ch1 == '.':
                    literal += '.'
                    self.stream.next()
                    ch1 = self.stream.peek()
                    while self.__isDigit(ch1):
                        ch = self.stream.next()
                        literal += ch
                        ch1 = self.stream.peek()
                    return Token(TokenKind.DecimalLiteral, text=literal)
                else:
                    ##无小数点，返回整型字面量
                    return Token(TokenKind.IntegerLiteral, literal)
            elif ch == '.':
                self.stream.next()
                ch1 = self.stream.peek()
                if self.__isDigit(ch1):
                    ##小数字面量
                    literal = '.'
                    while self.__isDigit(ch1):
                        ch = self.stream.next()
                        literal += ch
                        ch1 = self.stream.peek()
                    return Token(TokenKind.DecimalLiteral, text=literal)
                ##省略号
                elif ch1 == '.':
                    self.stream.next()
                    ch1 = self.stream.peek()
                    if ch1 == '.':
                        return Token(TokenKind.Seperator, text='...')
                    else:
                        print("Unrecognized pattern : .., missed a .?")
                        return self.__getAToken()
                ##.号分隔符
                else:
                    return Token(TokenKind.Seperator, text='.')
            elif ch == '/':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '*':
                    self.__skipMultipleLineComments()
                    return self.__getAToken()
                elif ch1 == '/':
                    self.__skipSingleLineComment()
                    return self.__getAToken()
                elif ch1 == '=':
                    self.stream.next()
                    return Token(TokenKind.Operator, '/=')
                else:
                    return Token(TokenKind.Operator, '/')
            elif ch == '+':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '+':
                    self.stream.next()
                    return Token(TokenKind.Operator, '++')
                elif ch1 == '=':
                    self.stream.next()
                    return Token(TokenKind.Operator, '+=')
                else:
                    return Token(TokenKind.Operator, '+')
            elif ch == '-':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '-':
                    self.stream.next()
                    return Token(kind=TokenKind.Operator, text='--')
                elif ch1 == '=':
                    self.stream.next()
                    return Token(kind=TokenKind.Operator, text='-=')
                else:
                    return Token(TokenKind.Operator, text='-')
            elif ch == '*':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    return Token(TokenKind.Operator, text='*=')
                else:
                    return Token(TokenKind.Operator, text='*')
            elif ch == '%':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    return Token(TokenKind.Operator, "%=")
                else:
                    return Token(TokenKind.Operator, "%")
            elif ch == '>':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    return Token(TokenKind.Operator, text='>=')
                elif ch1 == '>':
                    self.stream.next()
                    ch1 = self.stream.peek()
                    if ch1 == '>':
                        self.stream.next()
                        ch1 = self.stream.peek()
                        if ch1 == '=':
                            self.stream.next()
                            return Token(TokenKind.Operator, '>>>=')
                        else:
                            return Token(TokenKind.Operator, '>>>')
                    elif ch1 == '=':
                        self.stream.next()
                        return Token(TokenKind.Operator, '>>=')
                    else:
                        return Token(TokenKind.Operator, '>>')
                else:
                    return Token(TokenKind.Operator, '>')
            elif ch == '<':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    return Token(TokenKind.Operator, text='<=')
                elif ch1 == '<':
                    self.stream.next()
                    ch1 = self.stream.peek()
                    if ch1 == '=':
                        self.stream.next()
                        return Token(TokenKind.Operator, '<<=')
                    else:
                        return Token(TokenKind.Operator, '<<')
                else:
                    return Token(TokenKind.Operator, '<')
            elif ch == '=':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    ch1 = self.stream.peek()
                    if ch1 == '=':
                        self.stream.next()
                        return Token(TokenKind.Operator, '===')
                    else:
                        return Token(TokenKind.Operator, '==')
                elif ch1 == '>':
                    self.stream.next()
                    return Token(TokenKind.Operator, '=>')
                else:
                    return Token(TokenKind.Operator, '=')
            elif ch == '!':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    ch1 = self.stream.peek()
                    if ch1 == '=':
                        self.stream.next()
                        return Token(TokenKind.Operator, '!==')
                    else:
                        return Token(TokenKind.Operator, '!=')
                else:
                    return Token(TokenKind.Operator, '!')
            elif ch == '|':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '|':
                    self.stream.next()
                    return Token(TokenKind.Operator, '||')
                elif ch1 == '=':
                    self.stream.next()
                    return Token(TokenKind.Operator, '|=')
                else:
                    return Token(TokenKind.Operator, '|')
            elif ch == '&':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '&':
                    return Token(TokenKind.Operator, '&&')
                elif ch1 == '=':
                    self.stream.next()
                    return Token(TokenKind.Operator, '&=')
                else:
                    return Token(TokenKind.Operator, '&')
            elif ch == '^':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    return Token(TokenKind.Operator, '^=')
                else:
                    return Token(TokenKind.Operator, '^')
            elif ch == '~':
                self.stream.next()
                return Token(TokenKind.Operator, ch)
            else:
                print("Unrecognized pattern meeting ': "+ch+"', at"+self.stream.line+" col: "+self.stream.col)
                self.stream.next()
                return self.__getAToken()

    def __skipSingleLineComment(self):
        '''
        跳过两个 / 的单行注释
        先跳过第二个/ 往后一直找到 换行或者eof
        '''
        self.stream.next()
        while self.stream.peek() != '\n' and not self.stream.eof():
            self.stream.next()

    def __skipMultipleLineComments(self):
        '''
        跳过多行注释 /*，/之前已经处理过
        '''
        self.stream.next()
        if not self.stream.eof():
            ch1 = self.stream.next()
            while not self.stream.eof():
                ch2 = self.stream.next()
                if ch1 == '*' and ch2 == '/':
                    return
                ch1 = ch2
        print("Failed to find matching */ for multiple line comments at ': "+self.stream.line+" col: "+self.stream.col)

    def __skipWhiteSpaces(self):
        '''
        跳过空白字符
        '''
        while self.__isWhiteSpace(self.stream.peek()):
            self.stream.next()

    def __parseStringLiteral(self) -> Token:
        '''
        解析字符串,目前只支持双引号，且不支持转义
        '''
        token:Token = Token(TokenKind.StringLiteral, text="")
        ##第一个字符不用判断，因为前面判断过了
        self.stream.next()

        while not self.stream.eof() and self.stream.peek() != '"':
            token.text+=self.stream.next()

        if self.stream.peek()=='"':
            ##消化掉字符串末尾的引号
            self.stream.next()
        else:
            print("Expecting an \" at line: "+self.stream.line+" col: "+self.stream.col)

        return token

    def __parseIdentifier(self) -> Token:
        '''
        解析标识符，还要从标识符中挑出关键字
        '''
        token:Token = Token(TokenKind.Identifier, text='')

        ##第一个前面已经判断过了
        token.text+=self.stream.next()

        ##读入后序字符
        while not self.stream.eof() and self.__isLetterDigitOrUnderScore(self.stream.peek()):
            token.text+=self.stream.next()

        ##识别关键字
        if token.text in self.__KeyWords:
            token.kind = TokenKind.Keyword
        elif token.text == 'null':
            token.kind = TokenKind.NullLiteral
        elif token.text == 'true' or token.text == 'false':
            token.kind = TokenKind.BooleanLiteral

        return token

    def __isLetterDigitOrUnderScore(self, ch:str) -> bool:
        return (ch>='A' and ch<='z') or (ch>='a' and ch<='z') or (ch>='0' and ch<='9') or ch=='_'

    def __isLetter(self, ch:str) -> bool:
        return (ch>='A' and ch<='Z') or (ch>='a' and ch<='z')

    def __isDigit(self, ch:str) -> bool:
        return ch>='0' and ch<='9'

    def __isWhiteSpace(self, ch:str) -> bool:
        return ch == ' ' or ch == '\n' or ch == '\t'





