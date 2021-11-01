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
 * statement: block | expressionStatement | returnStatement | ifStatement | forStatement 
 *          | emptyStatement | functionDecl | variableDecl ;
 * block : '{' statementList? '}' ;
 * ifStatement : 'if' '(' expression ')' statement ('else' statement)? ;
 * forStatement : 'for' '(' (expression | 'let' variableDecl)? ';' expression? ';' expression? ')' statement ;
 * variableStatement : 'let' variableDecl ';';
 * variableDecl : Identifier typeAnnotation？ ('=' expression)? ;
 * typeAnnotation : ':' typeName;
 * functionDecl: "function" Identifier callSignature  block ;
 * callSignature: '(' parameterList? ')' typeAnnotation? ;
 * returnStatement: 'return' expression? ';' ;
 * emptyStatement: ';' ;
 * expressionStatement: expression ';' ;
 * expression: assignment;
 * assignment: binary (assignmentOp binary)* ;
 * binary: unary (binOp unary)* ;
 * unary: primary | prefixOp unary | primary postfixOp ;
 * primary: StringLiteral | DecimalLiteral | IntegerLiteral | functionCall | '(' expression ')' ;
 * assignmentOp = '=' | '+=' | '-=' | '*=' | '/=' | '>>=' | '<<=' | '>>>=' | '^=' | '|=' ;
 * binOp: '+' | '-' | '*' | '/' | '==' | '!=' | '<=' | '>=' | '<'
 *      | '>' | '&&'| '||'|...;
 * prefixOp = '+' | '-' | '++' | '--' | '!' | '~';
 * postfixOp = '++' | '--'; 
 * functionCall : Identifier '(' argumentList? ')' ;
 * argumentList : expression (',' expression)* ;
 '''

from scanner import Token, TokenKind, Scanner, Op, Seperator, Keyword, Position
from ast import AstVisitor, AstNode, Block, Prog, VariableStatement, VariableDecl, FunctionDecl, CallSignature, ParameterList ,FunctionCall, Statement, Expression, ExpressionStatement, Binary, Unary, IntegerLiteral, DecimalLiteral, StringLiteral, NullLiteral, BooleanLiteral, Variable, ReturnStatement, ErrorExp, ErrorStmt
from typing import List
from cyy_types import SysTypes, Type
from error import CompilerError

class Parser:
    def __init__(self, scanner:Scanner):
        self.scanner = scanner

        ##二元运算符优先级
        self.__opPrec:Dict[Op, int] = {
        Op.Assign:2, Op.PlusAssign:2, Op.MinusAssign:2, Op.MultiplyAssign:2, Op.DivideAssign:2,
        Op.ModulusAssign:2, Op.BitAndAssign:2, Op.BitOrAssign:2, Op.BitXorAssign:2, Op.LeftShiftArithmeticAssign:2, Op.RightShiftArithmeticAssign:2,
        Op.RightShiftLogicalAssign:2, Op.Or:4, Op.And:5, Op.BitOr:6, Op.BitXOr:7, Op.BitAnd:8, Op.EQ:9,
        Op.IdentityEquals:9, Op.NE:9, Op.IdentityNotEquals:9, Op.G:10, Op.GE:10, Op.L:10, Op.LE:10,
        Op.LeftShiftArithmetic:11, Op.RightShiftArithmetic:11, Op.RightShiftLogical:11, Op.Plus:12, Op.Minus:12, Op.Multiply:13, Op.Divide:13,
        Op.Modulus:13}

        self.errors:List[CompilerError] = []
        self.warnings:List[CompilerError] = []

    def addError(self, msg:str, pos:Position):
        self.errors.append(CompilerError(msg, pos, False))
        print("@"+pos.toString()+" : "+msg)

    def addWarning(self, msg:str, pos:Position):
        self.warnings.append(CompilerError(msg, pos, True))
        print("@"+pos.toString()+" : "+msg)

    def __getPrec(self, op:Op) -> int:
        ret = self.__opPrec.get(op)
        if ret is None:
            return -1
        else:
            return ret

    def parseProg(self) -> Prog:
        '''
        解析Prog prog=(functionDecl | functionCall)*
        '''
        beginPos = self.scanner.peek().pos
        stmts = self.parseStatementList()
        return Prog(beginPos, self.scanner.getLastPos(), stmts)

    def parseStatementList(self) -> List[Statement]:
        stmts:List[Statement] = []
        t = self.scanner.peek()
        ##statementList的Follow集合里有EOF和"}"这两个元素，分别用于prog和functionBody
        while t.kind != TokenKind.EOF and t.code != Seperator.CloseBrace:
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
        if t.kind == TokenKind.Keyword and t.code == Keyword.Function:
            return self.parseFunctionDecl()
        elif t.code == Keyword.Let:
            return self.parseVariableStatement()
        elif t.code == Keyword.Return:
            return self.parseReturnStatement()
        elif t.code == Seperator.OpenBrace:
            return self.parseBlock()
        elif t.kind == TokenKind.Identifier or\
                t.kind == TokenKind.DecimalLiteral or\
                t.kind == TokenKind.IntegerLiteral or\
                t.kind == TokenKind.StringLiteral or\
                t.code == Seperator.OpenParen:
            return self.parseExpressionStatement()
        else:
            self.addError("Can not recognize a expression starting with: "+self.scanner.peek().text, self.scanner.getLastPos())
            beginPos = self.scanner.getNextPos()
            self.__skip()
            quit()
            return ErrorStmt(beginPos, self.scanner.getLastPos())

    def parseReturnStatement(self)->ReturnStatement:
        '''
        return语句，无论是否出错都会返回一个return statement
        '''
        beginPos = self.scanner.getNextPos()
        exp:Expression or None = None

        #跳过'return'
        self.scanner.next()

        t = self.scanner.peek()
        if t.code != Seperator.SemiColon:#';'
            exp = self.parseExpression()

        #跳过';'
        t = self.scanner.peek()
        if t.code == Seperator.SemiColon:
            self.scanner.next()
        else:
            self.addError("Expecting ';' after return statement.",self.scanner.getLastPos())

        return ReturnStatement(beginPos, self.scanner.getLastPos(), exp)

    def parseVariableStatement(self)->VariableStatement:
        beginPos = self.scanner.getNextPos()
        isErrorNode = False
        #跳过'let'
        self.scanner.next()

        variableDecl = self.parseVariableDecl()

        t = self.scanner.peek()
        if t.code == Seperator.SemiColon:
            self.scanner.next()
        else:
            self.__skip()
            isErrorNode = True
        return VariableStatement(beginPos, self.scanner.getLastPos(), variableDecl, isErrorNode)

    def parseVariableDecl(self) -> VariableDecl:
        '''
        解析变量声明
        variableDecl: 'let'? Identifier typeAnnotation? ('=' singleExpression) ';'
        '''
        beginPos = self.scanner.getNextPos()

        t = self.scanner.next()
        if t.kind == TokenKind.Identifier:
            varName:str = t.text
            varType:str = 'any'
            init:Expression or None = None
            isErrorNode = False

            t1 = self.scanner.peek()
            if t1.code == Seperator.Colon:
                self.scanner.next()
                t1 = self.scanner.peek()
                if t1.kind == TokenKind.Identifier:
                    self.scanner.next()
                    varType = t1.text
                else:
                    self.addError("Error parsing type annotation in VariableDecl", self.scanner.getLastPos())
                    self.__skip(['='])
                    isErrorNode = True

            ##变量初始化部分
            t1 = self.scanner.peek()
            if t1.code == Op.Assign:
                self.scanner.next()
                init = self.parseExpression()
            
            return VariableDecl(beginPos, self.scanner.getLastPos(), varName, self.__parseType(varType), init, isErrorNode)

        else:
            self.addError("Expecting variable name in VariableDecl, while we meet "+t.text, self.scanner.getLastPos())
            self.__skip()
            return VariableDecl(beginPos, self.scanner.getLastPos(), "unknown", SysTypes.Any, None, True)

    def __parseType(self, typeName:str)->Type:
        if typeName=='any':
            return SysTypes.Any
        elif typeName=='number':
            return SysTypes.Number
        elif typeName=='boolean':
            return SysTypes.Boolean
        elif typeName=='string':
            return SysTypes.String
        elif typeName=='undefined':
            return SysTypes.Undefined
        elif typeName=='null':
            return SysTypes.Null
        elif typeName=='void':
            return SysTypes.Undefined
        else:
            self.addError("Unrecognized type:"+typeName, self.scanner.getLastPos())
            return SysTypes.Any

    def parseFunctionDecl(self) -> FunctionDecl:
        '''
        解析函数声明 
         * functionDecl: "function" Identifier callSignature  block ;
         * callSignature: '(' parameterList? ')' typeAnnotation? ;
         * parameterList : parameter (',' parameter)* ;
         * parameter : Identifier typeAnnotation? ;
         * block : '{' statementList? '}' ;
        返回None 说明解析出错
        '''
        beginPos = self.scanner.getNextPos()
        isErrorNode = False

        ##跳过关键字'function'
        self.scanner.next()

        t = self.scanner.next()
        if t.kind != TokenKind.Identifier:
            self.addError("Expecting a function name, while we got a "+t.text, self.scanner.getLastPos())
            self.__skip()
            isErrorNode = True

        ##解析callSignature
        callSignature:CallSignature = None
        t1 = self.scanner.peek()
        if t1.code == Seperator.OpenParen:
            callSignature = self.parseCallSignature()
        else:
            self.addError("Expecting '(' in FunctionDecl, while we got a "+t.text,self.scanner.getLastPos())
            self.__skip()
            callSignature = CallSignature(beginPos, self.scanner.getLastPos(),None,SysTypes.Any,True)

        #解析block
        functionBody:Block = None
        t1 = self.scanner.peek()
        if t1.code == Seperator.OpenBrace:
            functionBody = self.parseBlock()
        else:
            self.addError("Expecting '{' in FunctionDecl, while we got a "+t1.text,self.scanner.getLastPos())
            self.__skip()
            functionBody = Block(beginPos, self.scanner.getLastPos(),[],True)

        return FunctionDecl(beginPos,t.text,callSignature,functionBody,isErrorNode)

    def parseCallSignature(self)->CallSignature:
        beginPos = self.scanner.getNextPos()
        #跳过'('
        t = self.scanner.next()

        paramList = None
        if self.scanner.peek().code != Seperator.CloseParen:
            paramList = self.parseParameterList()

        t = self.scanner.peek()
        if t.code == Seperator.CloseParen:
            self.scanner.next()

            #解析typeAnnotation
            theType:str = 'any'
            if self.scanner.peek().code == Seperator.Colon:
                theType = self.parseTypeAnnotation()

            return CallSignature(beginPos, self.scanner.getLastPos(), paramList, self.__parseType(theType))
        else:
            self.addError("Expecting a ')' after for a call signature", self.scanner.getLastPos())
            return CallSignature(beginPos, self.scanner.getLastPos(), paramList, SysTypes.Any, True)

    def parseParameterList(self)->ParameterList:
        '''
        解析参数列表
        parameterList : parameter (',' parameter)* ;
        '''
        params:List[VariableDecl] = []
        beginPos = self.scanner.getNextPos()
        isErrorNode = False
        t = self.scanner.peek()
        while t.code != Seperator.CloseParen and t.kind != TokenKind.EOF:
            if t.kind == TokenKind.Identifier:
                self.scanner.next()
                t1 = self.scanner.peek()
                theType:str = "any"
                if t1.code == Seperator.Colon:
                    theType = self.parseTypeAnnotation()
                params.append(VariableDecl(beginPos, self.scanner.getLastPos(), t.text, self.__parseType(theType), None))
                ##处理','
                t = self.scanner.peek()
                if t.code != Seperator.CloseParen:
                    if t.code == Op.Comma:#','
                        self.scanner.next()#跳过','
                        t = self.scanner.peek()
                    else:
                        self.addError("Expecting a ',' or ')' after a parameter", self.scanner.getLastPos())
                        self.__skip()
                        isErrorNode = True
                        t2 = self.scanner.peek()
                        if t2.code == Op.Comma:
                            self.scanner.next()
                            t = self.scanner.peek()
                        else:
                            break
            else:
                self.addError("Expecting an Identifier as name of a parameter",self.scanner.getLastPos())
                self.__skip()
                isErrorNode=True
                if t.code == Op.Comma:
                    self.scanner.next()
                    t = self.scanner.peek()
                else:
                    break
        return ParameterList(beginPos, self.scanner.getLastPos(), params, isErrorNode)

    def parseTypeAnnotation(self)->str:
        '''
        解析类型注解，无论是否出错都会返回一个类型，默认any
        '''
        theType = 'any'
        ##跳过':'
        self.scanner.next()
        t = self.scanner.peek()
        if t.kind == TokenKind.Identifier:
            self.scanner.next()
            theType = t.text
        else:
            self.addError("Expecting a type name in the annotation",self.scanner.getLastPos())
        return theType

    def parseBlock(self)->Block:
        '''
        解析函数体 block:'{'statementList?'}'
        '''
        beginPos = self.scanner.getNextPos()
        t:Token = self.scanner.peek()
        #跳过'{'
        self.scanner.next()
        stmts = self.parseStatementList()
        t = self.scanner.peek()
        if t.code == Seperator.CloseBrace:
            self.scanner.next()
            return Block(beginPos, self.scanner.getLastPos(), stmts)
        else:
            self.addError("Expecting '}' while parsing a block, but we got a "+t.text,self.scanner.getLastPos())
            self.__skip()
            return Block(beginPos, self.scanner.getLastPos(), stmts, True)

    def parseExpressionStatement(self) -> ExpressionStatement:
        '''
        解析表达式语句
        '''
        exp = self.parseExpression()
        t = self.scanner.peek()
        stmt = ExpressionStatement(self.scanner.getLastPos(),exp)
        if t.code == Seperator.SemiColon:
            self.scanner.next()
        else:
            self.addError("Expecting a SemiColon at the end of an expression statement, while we got a "+t.text, self.scanner.getLastPos())
            self.__skip()
            stmt.endPos = self.scanner.getLastPos()
            stmt.isErrorNode = True
        return stmt

    def parseExpression(self) -> Expression:
        '''
        解析表达式
        '''
        return self.parseAssignment()

    def parseAssignment(self)->Expression:
        '''
        解析赋值表达式，右结合
        '''
        assignPrec = self.__getPrec(Op.Assign)
        ##先解析一个优先级更高的表达式
        exp1 = self.parseBinary(assignPrec)
        t = self.scanner.peek()
        tprec = self.__getPrec(t.code)
        #存放赋值运算符两边的表达式
        expStack:List[Expression] = []
        expStack.append(exp1)
        #存放赋值运算符
        opStack:List[Op] = []

        while t.kind == TokenKind.Operator and tprec == assignPrec:
            opStack.append(t.code)
            self.scanner.next()#跳过运算符
            exp1 = self.parseBinary(assignPrec)
            expStack.append(exp1)
            t = self.scanner.peek()
            tprec = self.__getPrec(t.code)

        #组装成右结合的AST
        exp1 = expStack[expStack.__len__() - 1]
        if opStack.__len__()>0:
            for i in range(expStack.__len__()-2,0):
                exp1 = Binary(opStack[i], expStack[i], exp1)

        return exp1

    def parseBinary(self, prec:int) -> Expression:
        '''
        采用运算符优先级算法，解析二元表达式
        递归算法，一开始提供的参数是最低优先级
        prec 当前运算符优先级
        '''
        exp1 = self.parseUnary()

        t = self.scanner.peek()
        tprec = self.__getPrec(t.code)
        '''
        ##只要右边出现的运算符优先级更高，就把右边出现的作为右子节点
        例如：
        2+3*5，第一次循环遇到+号，优先级大于0，一次递归binary
        递归时遇到乘号，优先级大于+，形成3*5返回，变成上一级的右子节点
        反过来 3*5+2 也一样
        '''
        while t.kind == TokenKind.Operator and tprec>prec:
            self.scanner.next()
            exp2 = self.parseBinary(tprec)
            exp:Binary = Binary(t.code, exp1, exp2)
            exp1 = exp
            t = self.scanner.peek()
            tprec = self.__getPrec(t.code)
        return exp1

    def parseUnary(self)->Expression:
        '''
        解析一元运算
        unary:primary | prefixOp unary | primary postfixOp
        '''
        beginPos = self.scanner.getNextPos()
        t = self.scanner.peek()
        #前缀的一元表达式
        if t.kind == TokenKind.Operator:
            self.scanner.next()
            exp = self.parseUnary()
            return Unary(beginPos, self.scanner.getLastPos(), t.code, exp, True)
        #后缀只能是++或--
        else:
            ##先解析一个primary
            exp = self.parsePrimary()
            t1 = self.scanner.peek()
            if t1.kind == TokenKind.Operator and (t1.code == Op.Inc or t1.code == Op.Dec):
                self.scanner.next()
                return Unary(beginPos, self.scanner.getLastPos(), t1.code, exp, False)
            else:
                return exp

    def parsePrimary(self) -> Expression:
        '''
        解析基础表达式
        '''
        beginPos = self.scanner.getNextPos()
        t = self.scanner.peek()
        # print("parsePrimary: "+t.text)

        ##以Identifier开头，可能是函数调用，也可能是变量，所以要看两个Token
        ##相当于使用了LL(2)算法
        if t.kind == TokenKind.Identifier:
            if self.scanner.peek2().code == Seperator.OpenParen:
                return self.parseFunctionCall()
            else:
                self.scanner.next()
                return Variable(beginPos, self.scanner.getLastPos(), t.text)
        elif t.kind == TokenKind.IntegerLiteral:
            self.scanner.next()
            return IntegerLiteral(beginPos, int(t.text))
        elif t.kind == TokenKind.DecimalLiteral:
            self.scanner.next()
            return DecimalLiteral(beginPos, float(t.text))
        elif t.kind == TokenKind.StringLiteral:
            self.scanner.next()
            r = StringLiteral(beginPos, t.text)
            return r
        elif t.code == Seperator.OpenParen:
            self.scanner.next()
            exp = self.parseExpression()
            t1 = self.scanner.peek()
            if t1.code == Seperator.CloseParen:
                self.scanner.next()
            else:
                self.addError("Expecting a ')' at the end of a primary expression, while we got a "+t.text, self.scanner.getLastPos())
                self.__skip()
            return exp
        else:
            ##理论上永远不会到达这里
            self.addError("Can not recognize a primary expression starting with: "+t.text, self.scanner.getLastPos())
            exp = ErrorExp(beginPos, self.scanner.getLastPos())
            return exp

    def parseFunctionCall(self) -> FunctionCall:
        '''
        解析函数调用 
        functionCall: Identifier '(' parameterList? ')'
        parameterList: StringLiteral (',' StringLiteral)*
        '''
        beginPos = self.scanner.getNextPos()
        params:List[Expression] = []
        name = self.scanner.next().text
        #跳过')'
        self.scanner.next()

        ##循环读出所有参数
        t1 = self.scanner.peek()
        while t1.code != Seperator.CloseParen and t1.kind != TokenKind.EOF:
            exp = self.parseExpression()

            params.append(exp)

            if exp.isErrorNode:
                self.addError("Error parsing parameter for function call "+name, self.scanner.getLastPos())

            t1 = self.scanner.peek()
            if t1.code != Seperator.CloseParen:
                if t1.code == Op.Comma:
                    t1 = self.scanner.next()
                else:
                    self.addError("Expecting a comma at the end of a parameter, while we got a "+t1.text, self.scanner.getLastPos())
                    self.__skip()
                    return FunctionCall(beginPos, self.scanner.getLastPos(), name, params, True)

        if t1.code == Seperator.CloseParen:
            self.scanner.next()

        return FunctionCall(beginPos, self.scanner.getLastPos(), name, params)

    def __skip(self, seperators:List[str]=[]):
        '''
        跳过一些Token, 用于错误恢复，以便继续解析后面Token
        '''
        t = self.scanner.peek()
        while t.kind != TokenKind.EOF:
            if t.kind == TokenKind.Keyword:
                return
            elif t.kind == TokenKind.Seperator and (t.text == ',' or t.text == ';' or t.text == '{' or t.text == '}' or t.text == '(' or t.text == ')' or t.text in seperators):
                return
            else:
                self.scanner.next()
                t = self.scanner.peek()
