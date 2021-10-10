'''
 * 语法分析器
 * @version 0.2
 * @author 程梓益
 * @license MIT License
 * 
当前特性：
简化版的函数声明
简化版的函数调用
简化版的表达式

当前语法规则：
 * prog = statementList? EOF;
 * statementList = (variableDecl | functionDecl | expressionStatement)+ ;
 * variableDecl : 'let' Identifier typeAnnotation？ ('=' singleExpression) ';';
 * typeAnnotation : ':' typeName;
 * functionDecl: "function" Identifier "(" ")"  functionBody;
 * functionBody : '{' statementList? '}' ;
 * statement: functionDecl | expressionStatement;
 * expressionStatement: expression ';' ;
 * expression: primary (binOP primary)* ;
 * primary: StringLiteral | DecimalLiteral | IntegerLiteral | functionCall | '(' expression ')' ;
 * binOP: '+' | '-' | '*' | '/' | '=' | '+=' | '-=' | '*=' | '/=' | '==' | '!=' | '<=' | '>=' | '<'
 *      | '>' | '&&'| '||'|...;
 * functionCall : Identifier '(' parameterList? ')' ;
 * parameterList : expression (',' expression)* ;
 '''

from scanner import Token, TokenKind, Scanner, CharStream
from ast import AstVisitor, AstNode, Block, Prog, VariableDecl,\
FunctionDecl, FunctionCall, Statement, Expression,\
ExpressionStatement, Binary, IntegerLiteral, DecimalLiteral,\
StringLiteral, NullLiteral, BooleanLiteral, Variable
from typing import List

class Parser:
    def __init__(self, scanner:Scanner):
        self.scanner = scanner
        ##运算符优先级
        self.__opPrec = {'=':2, '+=':2, '-=':2, '*=':2, '/=':2,
        '%=':2, '&=':2, '|=':2, '^=':2, '~=':2, '<<=':2, '>>=':2,
        '>>>=':2, '||':4, '&&':5, '|':6, '^':7, '&':8, '==':9,
        '===':9, '!=':9, '!==':9, '>':10, '>=':10, '<':10, '<=':10,
        '<<':11, '>>':11, '>>>':11, '+':12, '-':12, '*':13, '/':13,
        '%':13}

    def __getPrec(self, op:str) -> int:
        ret = self.__opPrec.get(op)
        if ret is None:
            return -1
        else:
            return ret

    def parseProg(self) -> Prog:
        '''
        解析Prog prog=(functionDecl | functionCall)*
        '''
        return Prog(self.parseStatementList())

    def parseStatementList(self) -> List[Statement]:
        stmts:List[Statement] = []
        t = self.scanner.peek()
        ##statementList的Follow集合里有EOF和"}"这两个元素，分别用于prog和functionBody
        while t.kind != TokenKind.EOF and t.text != '}':
            stmt = self.parseStatement()
            if stmt is not None:
                stmts.append(stmt)
            else:
                print("Error parsing a Statement in Programm.")
                return None
            t = self.scanner.peek()
        return stmts

    def parseStatement(self) -> Statement or None:
        '''
        解析语句
        函数调用，变量声明，变量赋值，都可能以Identifier开头
        所以预读一个Token不够，这里预读两个Token
        '''
        t = self.scanner.peek()
        if t.kind == TokenKind.Keyword and t.text == 'function':
            return self.parseFunctionDecl()
        elif t.text == 'let':
            return self.parseVariableDecl()
        elif t.kind == TokenKind.Identifier or\
                t.kind == TokenKind.DecimalLiteral or\
                t.kind == TokenKind.IntegerLiteral or\
                t.kind == TokenKind.StringLiteral or\
                t.text == '(':
            return self.parseExpressionStatement()
        else:
            print("Can not recognize a expression starting with: "+self.scanner.peek().text)
            return None

    def parseVariableDecl(self) -> VariableDecl or None:
        '''
        解析变量声明
        variableDecl: 'let'? Identifier typeAnnotation? ('=' singleExpression) ';'
        '''
        ##跳过'let'
        self.scanner.next()

        t = self.scanner.next()
        if t.kind == TokenKind.Identifier:
            varName:str = t.text
            varType:str = 'any'
            init:Expression or None = None

            t1 = self.scanner.peek()
            if t1.text == ':':
                self.scanner.next()
                t1 = self.scanner.peek()
                if t1.kind == TokenKind.Identifier:
                    self.scanner.next()
                    varType = t1.text
                    t1 = self.scanner.peek()
                else:
                    print("Error parsing type annotation in VariableDecl")
                    return None
            ##变量初始化部分
            if t1.text == '=':
                self.scanner.next()
                init = self.parseExpression()
            ##分号
            t1 = self.scanner.peek()
            if t1.text == ';':
                self.scanner.next()
                return VariableDecl(varName, varType, init)
            else:
                print("Expecting ; at the end of Variable declaration, while we meet "+t1.text)
                return None
        else:
            print("Expecting variable name in VariableDecl, while we meet "+t.text)
            return None

    def parseFunctionDecl(self) -> FunctionDecl or None:
        '''
        解析函数声明 functionDecl: "function" Identifier "(" ")" functionBody
        返回None 说明解析出错
        '''
        ##跳过'function'
        self.scanner.next()

        t = self.scanner.next()
        if t.kind == TokenKind.Identifier:
            t1 = self.scanner.next()
            if t1.text == "(":
                t2 = self.scanner.next()
                if t2.text==")":
                    functionBody = self.parseFunctionBody()
                    if functionBody is not None:
                        ##解析成功 从这里返回
                        return FunctionDecl(t.text, functionBody)
                    else:
                        print("Error parsing FunctionBody in FunctionDecl")
                        return None
                else:
                    print("Expecting ')' in FunctionDecl, while we got a ",t.text)
                    return None
            else:
                print("Expecting '(' in FunctionDecl, while we got a ",t.text)
                return None
        else:
            print("Expecting a function name, while we got a ",t.text)
            return None
        return None

    def parseFunctionBody(self) -> Block or None:
        '''
        解析函数体 functionBody: '{' functionCall* '}'
        '''
        t:Token = self.scanner.peek()
        if t.text == "{":
            self.scanner.next()
            stmts = self.parseStatementList()
            t = self.scanner.next()
            if t.text == "}":
                return Block(stmts)
            else:
                print("Expecting '}' in FunctionBody, while we got a "+t.text)
                return None
        else:
            print("Expecting '{' in FunctionBody, while we got a "+t.text)
            return None

    def parseExpressionStatement(self) -> ExpressionStatement or None:
        exp = self.parseExpression()
        if exp is not None:
            t = self.scanner.peek()
            if t.text == ';':
                self.scanner.next()
                return ExpressionStatement(exp)
            else:
                print("Expecting a semicolon at the end of an expression statement, while we got a "+t.text)
        else:
            print("Error parsing ExpressionStatement")
        return None

    def parseExpression(self) -> Expression or None:
        return self.parseBinary(0)

    def parseBinary(self, prec:int) -> Expression or None:
        '''
        采用运算符优先级算法，解析二元表达式
        递归算法，一开始提供的参数是最低优先级
        prec 当前运算符优先级
        '''
        exp1 = self.parsePrimary()
        if exp1 is not None:
            t = self.scanner.peek()
            tprec = self.__getPrec(t.text)

            '''
            ##只要右边出现的运算符优先级更高，就把右边出现的作为右子节点
            例如：
            2+3*5，第一次循环遇到+号，优先级大于0，一次递归binary
            递归时遇到乘号，优先级大于+，形成3*5返回，变成上一级的右子节点
            反过来 3*5+2 也一样
            '''
            while t.kind == TokenKind.Operator and tprec > prec:
                self.scanner.next()
                exp2 = self.parseBinary(tprec)
                if exp2 is not None:
                    exp:Binary = Binary(t.text, exp1, exp2)
                    exp1 = exp
                    t = self.scanner.peek()
                    tprec = self.__getPrec(t.text)
                else:
                    print("Can not recognize a expression starting with: "+t.text)
            return exp1
        else:
            print("Can not recognize a expression starting with: "+self.scanner.peek().text)
            return None

    def parsePrimary(self) -> Expression or None:
        '''
        解析基础表达式
        '''
        t = self.scanner.peek()
        print("parsePrimary: "+t.text)

        ##以Identifier开头，可能是函数调用，也可能是变量，所以要看两个Token
        ##相当于使用了LL(2)算法
        if t.kind == TokenKind.Identifier:
            if self.scanner.peek2().text == '(':
                return self.parseFunctionCall()
            else:
                self.scanner.next()
                return Variable(t.text)
        elif t.kind == TokenKind.IntegerLiteral:
            self.scanner.next()
            return IntegerLiteral(int(t.text))
        elif t.kind == TokenKind.DecimalLiteral:
            self.scanner.next()
            return DecimalLiteral(float(t.text))
        elif t.kind == TokenKind.StringLiteral:
            self.scanner.next()
            return StringLiteral(t.text)
        elif t.text == '(':
            self.scanner.next()
            exp = self.parseExpression()
            t1 = self.scanner.peek()
            if t1.text == ')':
                self.scanner.next()
                return exp
            else:
                print("Expecting a ')' at the end of a primary expression, while we got a "+t.text)
                return None
        else:
            print("Can not recognize a primary expression starting with: "+t.text)
            return None

    def parseFunctionCall(self) -> FunctionCall or None:
        '''
        解析函数调用 
        functionCall: Identifier '(' parameterList? ')'
        parameterList: StringLiteral (',' StringLiteral)*
        '''
        params:List[Expression] = []
        t:Token = self.scanner.next()
        if t.kind == TokenKind.Identifier:
            t1:Token = self.scanner.next()
            if t1.text == "(":
                ##循环读出所有参数
                t1 = self.scanner.peek()
                while t1.text != ")":
                    exp = self.parseExpression()
                    if exp is not None:
                        params.append(exp)
                    else:
                        print("Error parsing parameter in function call")
                        return None

                    t1 = self.scanner.peek()
                    if t1.text != ')':
                        if t1.text == ',':
                            t1 = self.scanner.next()
                        else:
                            print("Expecting a comma at the end of a function call, while we got a "+t1.text)
                            return None
                ##消耗掉')'
                self.scanner.next()
                return FunctionCall(t.text, params)

        return None

