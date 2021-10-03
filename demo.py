from typing import NamedTuple, List
from enum import Enum, unique
from abc import ABC, abstractmethod

## 枚举 Token的类型，关键字，标识，字符串，分隔符，操作符
@unique
class TokenKind(Enum):
    Keyword = 1
    Identifier = 2
    StringLiteral = 3
    Seperator = 4
    Operator = 5
    EOF = 6
##test
# [print(i.name, i.value) for i in TokenKind]

## Token数据结构，类型，内容
class Token(NamedTuple):
	kind: TokenKind
	text: str
##test
# t=Token(TokenKind.Keyword, "function")
# print(t)

##示例程序
'''
function sayHello(){
    println("Hello World!");
}
sayHello();
'''
##示例程序词法分析结果
tokenArray=[
    Token(TokenKind.Keyword, 'function'),
    Token(TokenKind.Identifier, 'sayHello'),
    Token(TokenKind.Seperator, '('),
    Token(TokenKind.Seperator, ')'),
    Token(TokenKind.Seperator, '{'),
    Token(TokenKind.Identifier, 'println'),
    Token(TokenKind.Seperator, '('),
    Token(TokenKind.StringLiteral, 'Hello World!'),
    Token(TokenKind.Seperator, ')'),
    Token(TokenKind.Seperator, ';'),
    Token(TokenKind.Seperator, '}'),
    Token(TokenKind.Identifier, 'sayHello'),
    Token(TokenKind.Seperator, '('),
    Token(TokenKind.Seperator, ')'),
    Token(TokenKind.Seperator, ';'),
    Token(TokenKind.EOF, '')
]
##test
# [print(i) for i in tokenArray]

##简化的词法分析器
class Tokenizer:
    def __init__(self, tokens:List[Token]):
        self.tokens = tokens
        self.pos = 0

    def next(self) -> Token:
        '''
        next方法，返回当前token后指针加1
        '''
        if (self.pos < len(self.tokens)):
            ret = self.tokens[self.pos]
            self.pos += 1
            return ret
        else:
            return self.tokens[self.pos-1]

    def position(self) -> int:
        return self.pos

    def traceBack(self, newPos:int) -> None:
        self.pos = newPos
##test
# tokenizer = Tokenizer(tokenArray)
# for i in range(len(tokenizer.tokens)+2):
#     t=tokenizer.next()
#     print(t, tokenizer.position())
# tokenizer.traceBack(0)
# print(tokenizer.position())

##语法分析 包括AST的数据结构和递归下降的语法解析程序

##基类
class AstNode(ABC):
    @abstractmethod
    def dump(self, prefix:str) -> None:
        pass

##语句 表示程序中的语句 子类包括函数申明和函数调用
class Statement(AstNode):
    pass

##程序节点 AST根节点 继承自AstNode
class Prog(AstNode):
    def __init__(self, stmts:List[Statement]):
        super()
        self.stmts = stmts

    def dump(self, prefix:str) -> None:
        print(prefix+"Prog")
        [x.dump(prefix+"\t") for x in self.stmts]

##函数调用 继承自Statement
class FunctionCall(Statement):
    def __init__(self, name:str, parameters:List[str]):
        super()
        self.name = name
        self.parameters = parameters
        self.definition = None

    def dump(self, prefix:str) -> None:
        res = ", resolved" if self.definition!=None else ", not resolved"
        print(prefix+"FunctionCall "+self.name+res)
        [print(prefix+"\t"+"Parameter: "+x) for x in self.parameters]

##函数体 继承自AstNode
class FunctionBody(AstNode):
    def __init__(self, stmts:List[FunctionCall]):
        super()
        self.stmts = stmts

    def dump(self, prefix:str) -> None:
        print(prefix+"FunctionBody")
        [x.dump(prefix+"\t") for x in self.stmts]

##函数声明 继承自statement
class FunctionDecl(Statement):
    def __init__(self, name:str, body:FunctionBody):
        super()
        self.name = name
        self.body = body

    def dump(self, prefix:str) -> None:
        print(prefix+"FunctionDecl "+self.name)
        self.body.dump(prefix+"\t")

class Parser:
    def __init__(self, tokenizer:Tokenizer):
        self.tokenizer = tokenizer

    def parseProg(self) -> Prog:
        '''
        解析Prog prog=(functionDecl | functionCall)*
        '''
        stmts:List[Statement] = []
        stmt:Statement = None
        while True:
            ## 每次解析一个语句，看是函数声明还是函数调用
            ##尝试函数声明
            stmt = self.parseFunctionDecl()
            if type(stmt) is FunctionDecl:
                stmts.append(stmt)
                continue

            ##尝试函数调用
            stmt = self.parseFunctionCall()
            if type(stmt) is FunctionCall:
                stmts.append(stmt)
                continue

            ##如果都不成功，结束
            if stmt is None:
                break

        return Prog(stmts)

    def parseFunctionDecl(self) -> FunctionDecl or None:
        '''
        解析函数声明 functionDecl: "function" Identifier "(" ")" functionBody
        '''
        oldPos:int = self.tokenizer.position()
        t:Token = self.tokenizer.next()
        ##读取关键字
        if t.kind == TokenKind.Keyword and t.text == "function":
            t = self.tokenizer.next()
            ## 读取标识符 和 分隔符
            if t.kind == TokenKind.Identifier:
                t1 = self.tokenizer.next()
                if t1.text == "(":
                    t2 = self.tokenizer.next()
                    if t2.text == ")":
                        functionBody = self.parseFunctionBody()
                        if functionBody is not None:
                            if type(functionBody) is FunctionBody:
                                return FunctionDecl(t.text, functionBody)
                            else:
                                print(f"WARRING:type of functionBody expect as FunctionBody but got {type(functionBody)}")
                    else:
                        print(f"Expecting ')' in FunctionDecl, while we got a {t.text}")
                        return
                else:
                    print(f"Expecting '(' in FunctionDecl, while we got a {t.text}")
                    return
        self.tokenizer.traceBack(oldPos)
        return None

    def parseFunctionBody(self) -> FunctionBody or None:
        '''
        解析函数体 functionBody: '{' functionCall* '}'
        '''
        oldPos:int = self.tokenizer.position()
        stmts:List[FunctionCall] = []
        t:Token = self.tokenizer.next()
        if t.text == "{":
            functionCall = self.parseFunctionCall()
            while functionCall is not None:
                stmts.append(functionCall)
                functionCall = self.parseFunctionCall()
            t = self.tokenizer.next()
            if t.text == "}":
                return FunctionBody(stmts)
            else:
                print("Expecting '}' in FunctionBody, while we got a {}".format(t.text))
                return
        else:
            print("Expecting '{' in FunctionBody, while we got a {}".format(t.text))
            return

        self.tokenizer.traceBack(oldPos)
        return None

    def parseFunctionCall(self) -> FunctionCall or None:
        '''
        解析函数调用 
        functionCall: Identifier '(' parameterList? ')'
        parameterList: StringLiteral (',' StringLiteral)*
        '''
        oldPos:int = self.tokenizer.position()
        params:List[str] = []
        t:Token = self.tokenizer.next()
        if t.kind == TokenKind.Identifier:
            t1:Token = self.tokenizer.next()
            if t1.text == "(":
                t2:Token = self.tokenizer.next()
                while t2.text != ")":
                    if t2.kind == TokenKind.StringLiteral:
                        params.append(t2.text)
                    else:
                        print(f"Expecting parameter in FunctionCall, while we got a {t2.text}")
                        return
                    t2 = self.tokenizer.next()
                    if t2.text != ")":
                        if t2.text == ",":
                            t2 = self.tokenizer.next()
                        else:
                            print(f"Expecting a comma in FunctionCall, while we got a {t2.text}")
                            return

                t2 = self.tokenizer.next()
                if t2.text == ";":
                    return FunctionCall(t.text, params)
                else:
                    print(f"Expecting a comma in FunctionCall, while we got a {t2.text}")
                    return

        self.tokenizer.traceBack(oldPos)
        return None

##基类，对AST做遍历，定义缺省的遍历方式，子类可重写
class AstVisitor:
    def visitProg(self, prog:Prog) -> any:
        retVal:any = None
        for x in prog.stmts:
            if type(x) == FunctionDecl:
                retVal = self.visitFunctionDecl(x)
            else:
                retVal = self.visitFunctionCall(x)
        return retVal

    def visitFunctionDecl(self, functionDecl:FunctionDecl) -> any:
        return self.visitFunctionBody(functionDecl.body)

    def visitFunctionBody(self, functionBody:FunctionBody) -> any:
        retVal:any = None
        for x in functionBody.stmts:
            retVal = self.visitFunctionCall(x)
        return retVal

    def visitFunctionCall(self, functionCall:FunctionCall) -> any:
        return "undefined"

##语义分析 对函数调用做引用消解，找到函数的声明
##遍历AST，如果发现函数调用就找它的定义
class RefResolver(AstVisitor):
    def __init__(self):
        self.prog:Prog or None = None

    def visitProg(self, prog:Prog) -> any:
        self.prog = prog
        for x in prog.stmts:
            if type(x) == FunctionCall:
                self.__resolveFunctionCall(prog, x)
            elif type(x) == FunctionDecl:
                self.visitFunctionDecl(x)

    def visitFunctionBody(self, functionBody:FunctionBody) -> any:
        assert type(functionBody)==FunctionBody
        if self.prog is not None:
            for x in functionBody.stmts:
                return self.__resolveFunctionCall(self.prog, x)

    def __resolveFunctionCall(self, prog:Prog, functionCall:FunctionCall):
        assert type(functionCall)==FunctionCall
        functionDecl = self.__findFunctionDecl(prog, functionCall.name)
        if functionDecl is not None:
            functionCall.definition = functionDecl
        else:
            if functionCall.name != "println":
                print(f"Error: cannot find definition of function {functionCall.name}")

    def __findFunctionDecl(self, prog:Prog, name:str) -> FunctionDecl or None:
        assert type(prog) is Prog
        for x in prog.stmts:
            if type(x) == FunctionDecl:
                functionDecl = x
                if functionDecl.body is not None and functionDecl.name == name:
                    return functionDecl
        return None

##解释器
##遍历AST，执行函数调用
class Interpretor(AstVisitor):
    def visitProg(self, prog:Prog) -> any:
        retVal:any = None
        for x in prog.stmts:
            if type(x) is FunctionCall:
                functionCall = x
                if functionCall.parameters is not None:
                    retVal = self.__runFunction(functionCall)
        return retVal

    def visitFunctionBody(self, functionBody:FunctionBody) -> any:
        assert type(functionBody)==FunctionBody
        retVal:any = None
        for x in functionBody.stmts:
            retVal = self.__runFunction(x)

    def __runFunction(self, functionCall:FunctionCall):
        assert type(functionCall) is FunctionCall
        if functionCall.name == "println":
            if len(functionCall.parameters) > 0:
                print(functionCall.parameters[0])
            else:
                print()
            return 0
        else:
            if functionCall.definition != None:
                self.visitFunctionBody(functionCall.definition.body)

##主程序
def compileAndRun():
    ##词法分析
    tokenizer = Tokenizer(tokenArray)
    print("\nToken:")
    for token in tokenArray:
        print(token)

    ##语法分析
    prog:Prog = Parser(tokenizer).parseProg()
    print("\nAST before RefResolve:")
    prog.dump("")

    ##语义分析
    RefResolver().visitProg(prog)
    print("\nAST RefResolved:")
    prog.dump("")

    print("\nrunning program:")
    retVal = Interpretor().visitProg(prog)
    print(f"return: {retVal}")

compileAndRun()



