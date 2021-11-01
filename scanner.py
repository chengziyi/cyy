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
    Seperator = 6
    Operator = 7
    EOF = 8

##Token的Code
##注意：几种类型的code的取值不能重叠。这样，由code就可以决定kind.
@unique
class Seperator(Enum):
    OpenBracket = 0,                #[
    CloseBracket = 1,                   #]
    OpenParen = 2,                      #(
    CloseParen = 3,                     #)
    OpenBrace = 4,                      #{
    CloseBrace = 5,                     #}
    Colon = 6,                          #:
    SemiColon = 7,                      #;

#运算符
@unique
class Op(Enum):
    QuestionMark = 100,             ##?   让几个类型的code取值不重叠
    Ellipsis = 0,                       ##...
    Dot = 1,                            ##.
    Comma = 2,                          ##,
    At = 3,                             ##@
    
    RightShiftArithmetic = 4,           ##>>
    LeftShiftArithmetic = 5,            ##<<
    RightShiftLogical = 6,              ##>>>
    IdentityEquals = 7,                 ##===
    IdentityNotEquals = 8,              ##!==

    BitNot = 9,                         ##~
    BitAnd = 10,                         ##&
    BitXOr = 11,                         ##^
    BitOr = 12,                          ##|

    Not = 13,                            ##!   
    And = 14,                            ##&&
    Or = 15,                             ##||

    Assign = 16,                         ##=
    MultiplyAssign = 17,                 ##*=
    DivideAssign = 18,                   ##/=
    ModulusAssign = 19,                  ##%=
    PlusAssign = 20,                     ##+=
    MinusAssign = 21,                    ##-=
    LeftShiftArithmeticAssign = 22,      ##<<=
    RightShiftArithmeticAssign = 23,     ##>>=
    RightShiftLogicalAssign = 24,        ##>>>=
    BitAndAssign = 25,                   ##&=
    BitXorAssign = 26,                   ##^=
    BitOrAssign = 27,                    ##|=
    
    ARROW = 28,                          ##=>

    Inc = 29,                            ##++
    Dec = 30,                            ##--

    Plus = 31,                           ##+
    Minus = 32,                          ##-
    Multiply = 33,                       ##*
    Divide = 34,                         ##/
    Modulus = 35,                        ##%
    
    EQ = 36,                             ##==
    NE = 37,                             ##!=
    G = 38,                              ##>
    GE = 39,                             ##>=
    L = 40,                              ##<
    LE = 41,                             ##<=

@unique
class Keyword(Enum):
    Function = 200,
    Class = 1,     
    Break = 2,       
    Delete = 3,    
    Return = 4,    
    Case = 5,      
    Do = 6,        
    If = 7,          
    Switch = 8,    
    Var = 9,
    Catch = 10,     
    Else = 11,      
    In = 12,          
    This = 13,      
    Void = 14,
    Continue = 15,  
    false = 16,     
    Instanceof = 17,  
    Throw = 18,     
    While = 19,
    Debugger = 20,  
    Finally = 21,   
    New = 22,         
    true = 23,      
    With = 24,  
    Default = 25,   
    For = 26,       
    Null = 27,        
    Try = 28,       
    Typeof = 29,
    #下面这些用于严格模式
    Implements = 30,
    Let = 31,       
    Private = 32,     
    Public = 33,    
    Yield = 34, 
    Interface = 35, 
    Package = 36,   
    Protected = 37,   
    Static = 38, 
    #more
    Any = 39,
    String = 40,
    Number = 41,
    Boolean = 42,
    Symbol = 43,
    #值
    Undefined = 44,

##对运算符的一些判断
class Operators:
    @staticmethod
    def isAssignOp(self, op:Op)->bool:
        return op>=Op.Assign and op<=Op.BitOrAssign

    @staticmethod
    def isRelationOp(self, op:Op)->bool:
        return op>=Op.EQ and op<=Op.LE

    @staticmethod
    def isArithmeticOp(self, op:Op)->bool:
        return op>=Op.Plus and op<=Op.Modulus

    @staticmethod
    def isLogicalOp(self, op:Op)->bool:
        return op>=Op.Not and op<=Op.Or

class Position:
    def __init__(self, begin:int, end:int, line:int, col:int):
        self.begin = begin  #开始字符
        self.end = end      #结束字符 
        self.line = line    #行
        self.col = col      #列

    def toString(self)->str:
        return "(ln:"+str(self.line)+", col:"+str(self.col)+", pos:"+str(self.begin)+")"

## Token数据结构，类型，内容
class Token:
    def __init__(self, kind:TokenKind, text:str, pos:Position, code:Op or Seperator or Keyword or None = None):
        self.kind = kind
        self.text = text
        self.pos = pos
        self.code = code

    def toString(self)->str:
        assert type(self.pos) is Position
        return "Token" + "@" + self.pos.toString()+"\t"+self.kind.name+"\t'"+self.text+"'"

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
        self.col:int = 1

    def peek(self) -> str:
        return self.data[self.pos] if self.pos<len(self.data) else ""

    def next(self) -> str:
        ch = self.data[self.pos]
        self.pos += 1
        if ch == "\n":
            self.line += 1
            self.col = 1
        else:
            self.col += 1
        return ch

    def eof(self) -> bool:
        return self.peek() == ''

    def getPosition(self)->Position:
        return Position(self.pos+1, self.pos+1, self.line, self.col)

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
        self.__lastPos:Position = Position(0,0,0,0)#这个Position是不合法的，只是为了避免null
        self.__KeywordMap:Dict[str, Keyword] = {
        "function":Keyword.Function,
        "class":Keyword.Class,
        "break":Keyword.Break,
        "delete":Keyword.Delete,
        "return":Keyword.Return,    
        "case":Keyword.Case,
        "do":Keyword.Do,
        "if":Keyword.If,
        "switch":Keyword.Switch,
        "var":Keyword.Var,
        "catch":Keyword.Catch,
        "else":Keyword.Else,
        "in":Keyword.In,
        "this":Keyword.This,
        "void":Keyword.Void,
        "continue":Keyword.Continue,
        "false":Keyword.false,
        "instanceof":Keyword.Instanceof,
        "throw":Keyword.Throw,
        "while":Keyword.While,
        "debugger":Keyword.Debugger,
        "finally":Keyword.Finally,
        "new":Keyword.New,
        "true":Keyword.true,
        "with":Keyword.With,  
        "default":Keyword.Default,
        "for":Keyword.For,
        "null":Keyword.Null,
        "try":Keyword.Try,
        "typeof":Keyword.Typeof,
        ##下面这些用于严格模式
        "implements":Keyword.Implements,
        "let":Keyword.Let,
        "private":Keyword.Private,
        "public":Keyword.Public,
        "yield":Keyword.Yield, 
        "interface":Keyword.Interface,
        "package":Keyword.Package,
        "protected":Keyword.Protected,
        "static":Keyword.Static,
        ##类型
        "number":Keyword.Number,
        "string":Keyword.String,
        "boolean":Keyword.Boolean,
        "any":Keyword.Any,
        "symbol":Keyword.Symbol,
        ##值
        "undefined":Keyword.Undefined}

    def next(self) -> Token:
        t:Token or None = self.tokens.pop(0) if len(self.tokens)>0 else None
        if t is None:
            t = self.__getAToken()
        self.__lastPos = t.pos
        assert type(t) is Token
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

    def getNextPos(self)->Position:
        ##获取接下来的Token位置
        return self.peek().pos

    def getLastPos(self)->Position:
        ##获取前一个Token的position
        return self.__lastPos

    def __getAToken(self) -> Token:
        '''
        从字符串流中获取一个新Token
        '''
        self.__skipWhiteSpaces()
        pos = self.stream.getPosition()
        if self.stream.eof():
            return Token(TokenKind.EOF, "EOF",pos)
        else:
            ch:str = self.stream.peek()
            if self.__isLetter(ch) or ch=='_':
                return self.__parseIdentifier()
            elif ch == '"':
                ##读到 " 时解析字符串
                return self.__parseStringLiteral()
            elif ch == '(':
                self.stream.next()
                return Token(TokenKind.Seperator, ch, pos, Seperator.OpenParen)
            elif ch == ')':
                self.stream.next()
                return Token(TokenKind.Seperator, ch, pos, Seperator.CloseParen)
            elif ch == '{':
                self.stream.next()
                return Token(TokenKind.Seperator, ch, pos, Seperator.OpenBrace)
            elif ch == '}':
                self.stream.next()
                return Token(TokenKind.Seperator, ch, pos, Seperator.CloseBrace)
            elif ch == '[':
                self.stream.next()
                return Token(TokenKind.Seperator, ch, pos, Seperator.OpenBracket)
            elif ch == ']':
                self.stream.next()
                return Token(TokenKind.Seperator, ch, pos, Seperator.CloseBracket)
            elif ch == ':':
                self.stream.next()
                return Token(TokenKind.Seperator, ch, pos, Seperator.Colon)
            elif ch == ';':
                self.stream.next()
                return Token(TokenKind.Seperator, ch, pos, Seperator.SemiColon)
            elif ch == '?':
                self.stream.next()
                return Token(TokenKind.Seperator, ch, pos, Op.QuestionMark)
            elif ch == '@':
                self.stream.next()
                return Token(TokenKind.Seperator, ch, pos, Op.At)
            elif ch == ',':
                self.stream.next()
                return Token(TokenKind.Seperator, ch, pos, Op.Comma)

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
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.DecimalLiteral, text=literal, pos=pos)
                else:
                    ##无小数点，返回整型字面量
                    return Token(TokenKind.IntegerLiteral, literal, pos)
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
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.DecimalLiteral, text=literal, pos=pos)
                ##省略号
                elif ch1 == '.':
                    self.stream.next()
                    ch1 = self.stream.peek()
                    if ch1 == '.':
                        pos.end = self.stream.pos+1
                        return Token(kind=TokenKind.Seperator, text='...', pos=pos, code=Op.Ellipsis)
                    else:
                        print("Unrecognized pattern : .., missed a .?")
                        return self.__getAToken()
                ##.号分隔符
                else:
                    return Token(TokenKind.Operator, text='.', pos=pos, code=Op.Dot)
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
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.Operator, '/=', pos, Op.DivideAssign)
                else:
                    return Token(TokenKind.Operator, '/', pos, Op.Divide)
            elif ch == '+':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '+':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.Operator, '++', pos, Op.Inc)
                elif ch1 == '=':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.Operator, '+=', pos, Op.PlusAssign)
                else:
                    return Token(TokenKind.Operator, '+', pos, Op.Plus)
            elif ch == '-':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '-':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(kind=TokenKind.Operator, text='--', pos=pos, code=Op.Dec)
                elif ch1 == '=':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(kind=TokenKind.Operator, text='-=', pos=pos, code=Op.MinusAssign)
                else:
                    return Token(kind=TokenKind.Operator, text='-', pos=pos, code=Op.Minus)
            elif ch == '*':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(kind=TokenKind.Operator, text='*=', pos=pos, code=Op.MultiplyAssign)
                else:
                    return Token(kind=TokenKind.Operator, text='*', pos=pos, code=Op.Multiply)
            elif ch == '%':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.Operator, "%=", pos, Op.ModulusAssign)
                else:
                    return Token(TokenKind.Operator, "%", pos, Op.Modulus)
            elif ch == '>':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(kind=TokenKind.Operator, text='>=', pos=pos, code=Op.GE)
                elif ch1 == '>':
                    self.stream.next()
                    ch1 = self.stream.peek()
                    if ch1 == '>':
                        self.stream.next()
                        ch1 = self.stream.peek()
                        if ch1 == '=':
                            self.stream.next()
                            pos.end = self.stream.pos+1
                            return Token(TokenKind.Operator, '>>>=', pos, Op.RightShiftLogicalAssign)
                        else:
                            pos.end = self.stream.pos+1
                            return Token(TokenKind.Operator, '>>>', pos, Op.RightShiftLogical)
                    elif ch1 == '=':
                        self.stream.next()
                        pos.end = self.stream.pos+1
                        return Token(TokenKind.Operator, '>>=', pos, Op.RightShiftArithmeticAssign)
                    else:
                        pos.end = self.stream.pos+1
                        return Token(TokenKind.Operator, '>>', Op.RightShiftArithmetic)
                else:
                    return Token(TokenKind.Operator, '>', pos, Op.G)
            elif ch == '<':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(kine=TokenKind.Operator, text='<=', pos=pos, code=Op.LE)
                elif ch1 == '<':
                    self.stream.next()
                    ch1 = self.stream.peek()
                    if ch1 == '=':
                        self.stream.next()
                        pos.end = self.stream.pos+1
                        return Token(TokenKind.Operator, '<<=', pos, Op.LeftShiftArithmeticAssign)
                    else:
                        pos.end = self.stream.pos+1
                        return Token(TokenKind.Operator, '<<', pos, Op.LeftShiftArithmetic)
                else:
                    return Token(TokenKind.Operator, '<', pos, Op.L)
            elif ch == '=':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    ch1 = self.stream.peek()
                    if ch1 == '=':
                        self.stream.next()
                        pos.end = self.stream.pos+1
                        return Token(TokenKind.Operator, '===', pos, Op.IdentityEquals)
                    else:
                        pos.end = self.stream.pos+1
                        return Token(TokenKind.Operator, '==', pos, Op.EQ)
                elif ch1 == '>':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.Operator, '=>', pos, Op.ARROW)
                else:
                    return Token(TokenKind.Operator, '=', pos, Op.Assign)
            elif ch == '!':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    ch1 = self.stream.peek()
                    if ch1 == '=':
                        self.stream.next()
                        pos.end = self.stream.pos+1
                        return Token(TokenKind.Operator, '!==', pos, Op.IdentityNotEquals)
                    else:
                        pos.end = self.stream.pos+1
                        return Token(TokenKind.Operator, '!=', pos, Op.NE)
                else:
                    return Token(TokenKind.Operator, '!', pos, Op.Not)
            elif ch == '|':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '|':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.Operator, '||', pos, Op.Or)
                elif ch1 == '=':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.Operator, '|=', pos, Op.BitOrAssign)
                else:
                    return Token(TokenKind.Operator, '|', pos, Op.BitOr)
            elif ch == '&':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '&':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.Operator, '&&', pos, Op.And)
                elif ch1 == '=':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.Operator, '&=', pos, Op.BitAndAssign)
                else:
                    return Token(TokenKind.Operator, '&', pos, Op.BitAnd)
            elif ch == '^':
                self.stream.next()
                ch1 = self.stream.peek()
                if ch1 == '=':
                    self.stream.next()
                    pos.end = self.stream.pos+1
                    return Token(TokenKind.Operator, '^=',pos, Op.BitXorAssign)
                else:
                    return Token(TokenKind.Operator, '^', pos, Op.BitXOr)
            elif ch == '~':
                self.stream.next()
                return Token(TokenKind.Operator, '~', pos, Op.BitNot)
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
        pos = self.stream.getPosition()
        token:Token = Token(TokenKind.StringLiteral, text="", pos=pos)
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
        pos = self.stream.getPosition()
        token:Token = Token(TokenKind.Identifier, text='', pos=pos)

        ##第一个前面已经判断过了
        token.text+=self.stream.next()

        ##读入后序字符
        while not self.stream.eof() and self.__isLetterDigitOrUnderScore(self.stream.peek()):
            token.text+=self.stream.next()

        pos.end = self.stream.pos+1

        ##识别关键字
        if token.text in self.__KeywordMap:
            token.kind = TokenKind.Keyword
            token.code = self.__KeywordMap.get(token.text)

        return token

    def __isLetterDigitOrUnderScore(self, ch:str) -> bool:
        return (ch>='A' and ch<='z') or (ch>='a' and ch<='z') or (ch>='0' and ch<='9') or ch=='_'

    def __isLetter(self, ch:str) -> bool:
        return (ch>='A' and ch<='Z') or (ch>='a' and ch<='z')

    def __isDigit(self, ch:str) -> bool:
        return ch>='0' and ch<='9'

    def __isWhiteSpace(self, ch:str) -> bool:
        return ch == ' ' or ch == '\n' or ch == '\t'



