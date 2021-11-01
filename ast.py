'''
 * AST
 * @version 0.2
 * @author 程梓益
 * @license MIT License
 * 
'''

from typing import List
from abc import ABC, abstractmethod
from cyy_types import Type, SysTypes
from symbol import Symbol, FunctionSymbol, VarSymbol, built_ins
# from scope import Scope
from scanner import Position, Op

##parser
##AST基类
class AstNode(ABC):
    def __init__(self, beginPos:Position, endPos:Position, isErrorNode:bool):
        self.beginPos = beginPos
        self.endPos = endPos
        self.isErrorNode = isErrorNode

    def accept(self, visitor, additional:any) -> any:
        '''
        visitor 模式中 用于接受visitor的访问
        visitor:AstVisitor
        '''
        pass

##语句 表示程序中的语句 包括函数声明，表达式语句
class Statement(AstNode):
    pass

##函数体
class Block(Statement):
    def __init__(self, beginPos:Position, endPos:Position, stmts:List[Statement], isErrorNode:bool = False):
        super().__init__(beginPos, endPos, isErrorNode)
        self.stmts = stmts
        self.scope = None #scope:Scope or None

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitBlock(self, additional)

##程序节点 AST根节点
class Prog(Block):
    def __init__(self, beginPos:Position, endPos:Position, stmts:List[Statement]):
        super().__init__(beginPos, endPos, stmts, False)
        self.stmts = stmts
        self.sym:FunctionSymbol or None = None

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitProg(self, additional)

##声明 每个声明都会对应一个符号
class Decl(AstNode):
    def __init__(self, beginPos:Position, endPos:Position, name:str, isErrorNode:bool):
        super().__init__(beginPos, endPos, isErrorNode)
        self.name = name

##表达式
class Expression(AstNode):
    theType:Type or None = None
    #当前位置需要一个左值。赋值符号、点符号的左边，需要左值
    shouldBeLeftValue:bool = False
    #是否是一个左值
    isLeftValue:bool = False
    constValue:any = None #常量

    inferredType:Type or None = None

##变量声明节点
class VariableDecl(Decl):
    def __init__(self, beginPos:Position, endPos:Position, name:str, theType:Type, init:Expression or None, isErrorNode:bool = False):
        super().__init__(beginPos, endPos, name, isErrorNode)
        self.theType = theType
        self.init = init
        self.sym:VarSymbol or None = None
        self.inferredType:Type or None = None

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitVariableDecl(self, additional)

class ParameterList(AstNode):
    def __init__(self, beginPos:Position, endPos:Position, params:List[VariableDecl], isErrorNode:bool = False):
        super().__init__(beginPos, endPos, isErrorNode)
        self.params = params

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitParameterList(self, additional)

class CallSignature(AstNode):
    def __init__(self, beginPos:Position, endPos:Position, paramList:ParameterList or None, theType:Type, isErrorNode:bool = False):
        super().__init__(beginPos, endPos, isErrorNode)
        self.paramList = paramList
        self.theType = theType

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitCallSignature(self, additional)

##函数声明
class FunctionDecl(Decl):
    def __init__(self, beginPos:Position, name:str, callSignature:CallSignature, body:Block, isErrorNode:bool = False):
        super().__init__(beginPos, body.endPos, name, isErrorNode)
        self.callSignature = callSignature
        self.body = body #函数体
        self.scope = None #该函数对应Scope
        self.sym:FunctionSymbol or None = None

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitFunctionDecl(self, additional)

##变量声明语句
class VariableStatement(Statement):
    def __init__(self, beginPos:Position, endPos:Position, variableDecl:VariableDecl, isErrorNode:bool=False):
        super().__init__(beginPos, endPos, isErrorNode)
        self.variableDecl = variableDecl

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitVariableStatement(self, additional)

##二元表达式
class Binary(Expression):
    def __init__(self, op:Op, exp1:Expression, exp2:Expression, isErrorNode:bool = False):
        super(exp1.beginPos, exp2.endPos, isErrorNode)
        self.op = op
        self.exp1 = exp1
        self.exp2 = exp2

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitBinary(self, additional)

class Unary(Expression):
    def __init__(self, beginPos:Position, endPos:Position, op:Op, exp:Expression, isPrefix:bool, isErrorNode:bool = False):
        super(beginPos, endPos, isErrorNode)
        self.op = op #运算符
        self.exp = exp #表达式
        self.isPrefix = isPrefix #前缀 后缀

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitUnary(self, additional)

##表达式语句 表达式后面加个分号
class ExpressionStatement(Statement):
    def __init__(self, endPos:Position, exp:Expression, isErrorNode:bool = False):
        super().__init__(exp.beginPos, endPos, isErrorNode)
        self.exp = exp

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitExpressionStatement(self, additional)

##return 语句
class ReturnStatement(Statement):
    def __init__(self, beginPos:Position, endPos:Position, exp:Expression or None, isErrorNode:bool = False):
        super().__init__(beginPos, endPos, isErrorNode)
        self.exp = exp

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitReturnStatement(self, additional)

##if 语句
class IfStatement(Statement):
    def __init__(self, beginPos:Position, endPos:Position, condition:Expression, stmt:Statement, elseStmt:Statement or None, isErrorNode:bool = False):
        super().__init__(beginPos, endPos, isErrorNode)
        self.condition = condition
        self.stmt = stmt
        self.elseStmt = elseStmt

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitIfStatement(self, additional)

##for 语句
class ForStatement(Statement):
    def __init__(self, beginPos:Position, endPos:Position, init:Expression or VariableDecl or None, termination:Expression or None, increment:Expression or None, stmt:Statement, isErrorNode:bool = False):
        super().__init__(beginPos, endPos, isErrorNode)
        self.init = init
        self.condition = termination
        self.increment = increment
        self.stmt = stmt
        self.scope = None

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitForStatement(self, additional)

##函数调用
class FunctionCall(Expression):
    def __init__(self, beginPos:Position, endPos:Position, name:str, paramValues:List[Expression], isErrorNode:bool = False):
        assert type(paramValues) is list,f"paramValues must be list,not {type(parameters)}"
        super().__init__(beginPos, endPos, isErrorNode)
        self.name = name
        self.arguments = paramValues
        self.sym:FunctionSymbol or None = None

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor

        return visitor.visitFunctionCall(self, additional)

##变量引用
class Variable(Expression):
    def __init__(self, beginPos:Position, endPos:Position, name:str, isErrorNode:bool = False):
        super(beginPos, endPos, isErrorNode)
        self.name = name
        self.sym:VarSymbol or None = None

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitVariable(self, additional)

##字符串字面量
class StringLiteral(Expression):
    def __init__(self, pos:Position, value:str, isErrorNode:bool = False):
        super().__init__(pos, pos, isErrorNode)
        self.value = value
        self.theType = SysTypes.String
        self.constValue = value

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitStringLiteral(self, additional)

##整型字面量
class IntegerLiteral(Expression):
    def __init__(self, pos:Position, value:int, isErrorNode:bool = False):
        super().__init__(pos, pos, isErrorNode)
        self.value = value
        self.theType = SysTypes.Integer
        self.constValue = value

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitIntegerLiteral(self, additional)

##实数字面量
class DecimalLiteral(Expression):
    def __init__(self, pos:Position, value:int, isErrorNode:bool = False):
        super().__init__(pos, pos, isErrorNode)
        self.value = value
        self.theType = SysTypes.Decimal
        self.constValue = value

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitDecimalLiteral(self, additional)

##null字面量
class NullLiteral(Expression):
    def __init__(self, pos:Position, isErrorNode:bool = False):
        super().__init__(pos, pos, isErrorNode)
        self.theType = SysTypes.Null
        self.value:None = None
        self.constValue = self.value

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitNullLiteral(self, additional)

##boolean字面量
class BooleanLiteral(Expression):
    def __init__(self, pos:Position, value:bool, isErrorNode:bool = False):
        super().__init__(pos, pos, isErrorNode)
        self.value = value
        self.theType = SysTypes.Boolean
        self.constValue = value

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitBooleanLiteral(self, additional)

##错误表达式
class ErrorExp(Expression):
    def __init__(self, beginPos:Position, endPos:Position):
        super().__init__(beginPos, endPos, True)
        self.isErrorNode = True

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitErrorExp(self, additional)

##错误语句
class ErrorStmt(Statement):
    def __init__(self, beginPos:Position, endPos:Position):
        super().__init__(beginPos, endPos, True)
        self.isErrorNode = True

    def accept(self, visitor, additional:any) -> any:
        #visitor:AstVisitor
        return visitor.visitErrorStmt(self, additional)

#visitor
##基类，对AST做遍历，定义缺省的遍历方式，子类可重写
class AstVisitor:
    def visit(self, node:AstNode, additional:any=None) -> any:
        '''
        对抽象类的访问，相应的具体类会调用visitor具体的方法
        '''

        return node.accept(self, additional)

    def visitProg(self, prog:Prog, additional:any = None) -> any:
        assert type(prog) is Prog
        return self.visitBlock(prog, additional)

    def visitVariableStatement(self, variableStmt:VariableStatement, additional:any=None):
        return self.visit(variableStmt.variableDecl, additional)

    def visitVariableDecl(self, variableDecl:VariableDecl, additional:any=None) -> any:
        assert type(variableDecl) is VariableDecl
        if variableDecl.init is not None:
            return self.visit(variableDecl.init, additional)

    def visitFunctionDecl(self, functionDecl:FunctionDecl, additional:any=None) -> any:
        assert type(functionDecl) is FunctionDecl
        self.visit(functionDecl.callSignature, additional)
        return self.visit(functionDecl.body, additional)

    def visitCallSignature(self, callSignature:CallSignature, additional:any=None)->any:
        if callSignature.paramList is not None:
            return self.visit(callSignature.paramList, additional)

    def visitParameterList(self, paramList:ParameterList, additional:any=None)->any:
        retVal:any = None
        for x in paramList.params:
            retVal = self.visit(x, additional)
        return retVal

    def visitBlock(self, block:Block, additional:any = None) -> any:
        # assert type(block) is Block,f"type of block is {type(block)}"
        retVal:any = None
        for x in block.stmts:
            retVal = self.visit(x, additional)
        return retVal

    def visitExpressionStatement(self, stmt:ExpressionStatement, additional:any=None) -> any:
        assert type(stmt) is ExpressionStatement
        return self.visit(stmt.exp, additional)

    def visitReturnStatement(self, stmt:ReturnStatement, additional:any=None):
        if stmt.exp is not None:
            return self.visit(stmt.exp, additional)

    def visitIfStatement(self, stmt:IfStatement, additional:any=None):
        self.visit(stmt.condition, additional)
        self.visit(stmt.stmt, additional)
        if stmt.elseStmt is not None:
            self.visit(stmt.elseStmt, additional)

    def visitForStatement(self, stmt:ForStatement, additional:any=None):
        if stmt.init is not None:
            self.visit(stmt.init, additional)
        if stmt.condition is not None:
            self.visit(stmt.condition, additional)
        if stmt.increment is not None:
            self.visit(stmt.increment, additional)
        self.visit(stmt.stmt, additional)

    def visitBinary(self, exp:Binary, additional:any=None) -> any:
        assert type(exp) is Binary
        self.visit(exp.exp1, additional)
        self.visit(exp.exp2, additional)

    def visitUnary(self, exp:Unary, additional:any=None):
        self.visit(exp.exp, additional)

    def visitIntegerLiteral(self, exp:IntegerLiteral, additional:any=None) -> any:
        assert type(exp) is IntegerLiteral
        return exp.value

    def visitDecimalLiteral(self, exp:DecimalLiteral, additional:any=None) -> any:
        assert type(exp) is DecimalLiteral
        return exp.value

    def visitStringLiteral(self, exp:StringLiteral, additional:any=None) -> any:
        assert type(exp) is StringLiteral
        return exp.value

    def visitNullLiteral(self, exp:NullLiteral, additional:any=None) -> any:
        assert type(exp) is NullLiteral
        return exp.value

    def visitBooleanLiteral(self, exp:BooleanLiteral, additional:any=None) -> any:
        assert type(exp) is BooleanLiteral
        return exp.value

    def visitVariable(self, variable:Variable, additional:any=None) -> any:
        assert type(variable) is Variable
        return "undefined"

    def visitFunctionCall(self, functionCall:FunctionCall, additional:any=None) -> any:
        assert type(functionCall) is FunctionCall
        for param in functionCall.arguments:
            self.visit(param, additional)
        return "undefined"

    def visitErrorExp(self, errorNode:ErrorExp, additional:any=None):
        return "undefined"

    def visitErrorStmt(self, errorStmt:ErrorStmt, additional:any=None):
        return "undefined"

##AST调试信息
class AstDumper(AstVisitor):
    def visitProg(self, prog:Prog, prefix:any)->any:
        err = " **E** " if prog.isErrorNode else ""
        print(prefix + "Prog" + err)
        for x in prog.stmts:
            self.visit(x, prefix+"    ")

    def visitVariableStatement(self, variableStmt:VariableStatement, prefix:any):
        err = " **E** " if variableStmt.isErrorNode else ""
        print(prefix+"VariableStatement "+err)
        self.visit(variableStmt.variableDecl, prefix+"    ")

    def visitVariableDecl(self, variableDecl:VariableDecl, prefix:any)->any:
        thetype = "" if variableDecl.theType is None else "("+variableDecl.theType.name+")"
        err = " **E** " if variableDecl.isErrorNode else ""
        print(prefix+"VariableDecl "+variableDecl.name + thetype + err)
        if variableDecl.init is None:
            print(prefix+"no initialization.")
        else:
            self.visit(variableDecl.init, prefix+"    ")

    def visitFunctionDecl(self, functionDecl:FunctionDecl, prefix:any):
        err = " **E** " if functionDecl.isErrorNode else ""
        print(prefix+"FunctionDecl "+ functionDecl.name + err)
        self.visit(functionDecl.callSignature, prefix+"    ")
        self.visit(functionDecl.body, prefix+"    ")

    def visitCallSignature(self, callSinature:CallSignature, prefix:any):
        err = " **E** " if callSinature.isErrorNode else ""
        print(prefix+ err +"Return type: " + callSinature.theType.name)
        if callSinature.paramList is not None:
            self.visit(callSinature.paramList, prefix + "    ")

    def visitParameterList(self, paramList:ParameterList, prefix:any):
        err = " **E** " if paramList.isErrorNode else ""
        param = "none" if paramList.params.length==0 else ""
        print(prefix+"ParamList:" + err + param)
        for x in paramList.params:
            self.visit(x, prefix+"    ")

    def visitBlock(self, block:Block, prefix:any):
        if block.isErrorNode:
            print(prefix + "Block" +  " **E** ")

        for x in block.stmts:
            self.visit(x, prefix+"    ")
    
    def visitExpressionStatement(self, stmt: ExpressionStatement, prefix:any):
        print(prefix+"ExpressionStatement" + " **E** " if stmt.isErrorNode else "")
        return self.visit(stmt.exp, prefix+"    ")

    def visitReturnStatement(self, stmt:ReturnStatement, prefix:any):
        print(prefix+"ReturnStatement" + " **E** " if stmt.isErrorNode else "")
        if stmt.exp is not None:
            return self.visit(stmt.exp, prefix+"    ")

    def visitIfStatement(self, stmt:IfStatement, prefix:any):
        print(prefix+"IfStatement" + " **E** " if stmt.isErrorNode else "")
        print(prefix+"    Condition:")
        self.visit(stmt.condition, prefix+"    ")
        print(prefix+"    Then:")
        self.visit(stmt.stmt, prefix+"    ")
        if stmt.elseStmt is not None:
            print(prefix+"    Else:")
            self.visit(stmt.elseStmt, prefix+"    ")

    def visitForStatement(self, stmt:ForStatement, prefix:any):
        print(prefix+"ForStatement" + " **E** " if stmt.isErrorNode else "")
        if stmt.init is not None:
            print(prefix+"    Init:")
            self.visit(stmt.init, prefix+"    ")

        if stmt.condition is not None:
            print(prefix+"    Condition:")
            self.visit(stmt.condition, prefix+"    ")

        if stmt.increment is not None:
            print(prefix+"    Increment:")
            self.visit(stmt.increment, prefix+"    ")

        print(prefix+"    Body:")
        self.visit(stmt.stmt, prefix+"    ")

    def visitBinary(self, exp:Binary, prefix:any):
        thetype = "" if exp.theType is None else "("+exp.theType.name+")"
        err = " **E** " if exp.isErrorNode else ""
        print(prefix+"Binary:"+Op[exp.op]+ thetype + err)
        self.visit(exp.exp1, prefix+"    ")
        self.visit(exp.exp2, prefix+"    ")

    def visitUnary(self, exp:Unary, prefix:any):
        print(prefix + "Prefix " if exp.isPrefix else "PostFix " +"Unary:"+Op[exp.op]+ ( "" if exp.theType is None else "("+exp.theType.name+")") + " **E** " if exp.isErrorNode else "")
        self.visit(exp.exp, prefix+"    ")

    def visitIntegerLiteral(self, exp:IntegerLiteral, prefix:any):
        print(prefix+exp.value + "" if exp.theType is None else "("+exp.theType.name+")" + " **E** " if exp.isErrorNode else "")

    def visitDecimalLiteral(self, exp:DecimalLiteral, prefix:any):
        print(prefix+exp.value+ "" if exp.theType is None else "("+exp.theType.name+")"+ " **E** " if exp.isErrorNode else "")

    def visitStringLiteral(self, exp:StringLiteral, prefix:any):
        print(prefix+exp.value+ "" if exp.theType is None else "("+exp.theType.name+")"+ " **E** " if exp.isErrorNode else "")

    def visitNullLiteral(self, exp:NullLiteral, prefix:any):
        print(prefix+exp.value+ "" if exp.theType is None else "("+exp.theType.name+")"+ " **E** " if exp.isErrorNode else "")

    def visitBooleanLiteral(self, exp:BooleanLiteral, prefix:any):
        print(prefix+exp.value+ "" if exp.theType is None else "("+exp.theType.name+")"+ " **E** " if exp.isErrorNode else "")

    def visitVariable(self, variable:Variable, prefix:any):
        print(prefix+"Variable: "+ " **E** " if variable.isErrorNode else ""+variable.name + "" if variable.theType is None else "("+variable.theType.name+")" + ", LeftValue" if variable.isLeftValue else "" + ", resolved" if variable.sym is not None else ", not resolved")

    def visitFunctionCall(self, functionCall:FunctionCall, prefix:any):
        thetype = "" if functionCall.theType is None else "("+functionCall.theType.name+")"
        err = " **E** " if functionCall.isErrorNode else ""
        resolve = ", resolved" if functionCall.sym is not None else ", not resolved"
        builtin_or_resolve = ', built-in' if functionCall.name in built_ins else resolve
        print(prefix+"FunctionCall "+ thetype + err +functionCall.name + builtin_or_resolve)
        for param in functionCall.arguments:
            self.visit(param, prefix+"    ")

    def visitErrorExp(self, errorNode:ErrorExp, prefix:any):
        print(prefix+"Error Expression **E**")

    def visitErrorStmt(self, errorStmt:ErrorStmt, prefix:any):
        print(prefix+"Error Statement **E**")



