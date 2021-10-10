'''
 * AST
 * @version 0.2
 * @author 程梓益
 * @license MIT License
 * 
'''

from typing import List
from abc import ABC, abstractmethod

##语法分析
##基类
class AstNode(ABC):
    @abstractmethod
    def dump(self, prefix:str) -> None:
        pass

    def accept(self, visitor) -> any:
        pass

##语句 表示程序中的语句
class Statement(AstNode):
    pass

##表达式
class Expression(AstNode):
    pass

##函数体 继承自AstNode
class Block(AstNode):
    def __init__(self, stmts:List[Statement]):
        super()
        self.stmts = stmts

    def accept(self, visitor) -> any:
        return visitor.visitBlock(self)

    def dump(self, prefix:str) -> None:
        print(prefix+"Block")
        [x.dump(prefix+"    ") for x in self.stmts]

##程序节点 AST根节点
class Prog(Block):
    def accept(self, visitor) -> any:
        return visitor.visitProg(self)

    def dump(self, prefix:str) -> None:
        print(prefix+"Prog")
        [x.dump(prefix+"    ") for x in self.stmts]

##声明 每个声明都会对应一个符号
class Decl(ABC):
    def __init__(self, name:str):
        self.name = name

##函数声明
class FunctionDecl(Decl):
    def __init__(self, name:str, body:Block):
        super(name).__init__(name)
        self.body = body

    def accept(self, visitor) -> any:
        return visitor.visitFunctionDecl(self)

    def dump(self, prefix:str) -> None:
        print(prefix+"FunctionDecl "+self.name)
        self.body.dump(prefix+"    ")

##变量声明
class VariableDecl(Decl):
    def __init__(self, name:str, varType:str, init:Expression or None):
        super().__init__(name)
        self.varType:str = varType
        self.init:Expression or None = init

    def accept(self, visitor) -> any:
        return visitor.visitVariableDecl(self)

    def dump(self, prefix:str) -> None:
        print(prefix+"VariableDecl "+self.name+", type: "+self.varType)
        if self.init is None:
            print(prefix+"no initialization.")
        else:
            self.init.dump(prefix+'    ')

##二元表达式
class Binary(Expression):
    def __init__(self, op:str, exp1:Expression, exp2:Expression):
        super()
        self.op = op
        self.exp1 = exp1
        self.exp2 = exp2

    def accept(self, visitor) -> any:
        return visitor.visitBinary(self)

    def dump(self, prefix:str) -> None:
        print(prefix+"Binary:"+self.op)
        self.exp1.dump(prefix+"    ")
        self.exp2.dump(prefix+"    ")

##表达式语句 表达式后面加个分号
class ExpressionStatement(Statement):
    def __init__(self, exp:Expression):
        super()
        self.exp = exp

    def accept(self, visitor) -> any:
        return visitor.visitExpressionStatement(self)

    def dump(self, prefix:str) -> None:
        print(prefix+"ExpressionStatement")
        self.exp.dump(prefix+"    ")

##函数调用 继承自Statement
class FunctionCall(AstNode):
    def __init__(self, name:str, parameters:List[Expression]):
        assert type(parameters) is list,f"parameters must be list,not {type(parameters)}"
        super()
        self.name = name
        self.parameters = parameters
        self.decl:FunctionDecl or None = None

    def accept(self, visitor) -> any:
        return visitor.visitFunctionCall(self)

    def dump(self, prefix:str) -> None:
        res = ", resolved" if self.decl is not None else ", not resolved"
        print(prefix+"FunctionCall "+self.name+res)
        [x.dump(prefix+"  ") for x in self.parameters]

##变量引用
class Variable(Expression):
    def __init__(self, name:str):
        super()
        self.name = name
        self.decl:VariableDecl or None = None

    def accept(self, visitor) -> any:
        return visitor.visitVariable(self)

    def dump(self, prefix:str) -> None:
        res = ", resolved" if self.decl is not None else ", not resolved"
        print(prefix+"Variable:"+self.name+res)

##字符串字面量
class StringLiteral(Expression):
    def __init__(self, value:str):
        super()
        self.value = value

    def accept(self, visitor) -> any:
        return visitor.visitStringLiteral(self)

    def dump(self, prefix:str) -> None:
        print(prefix+self.value)

##整型字面量
class IntegerLiteral(Expression):
    def __init__(self, value:int):
        super()
        self.value = value

    def accept(self, visitor) -> any:
        return visitor.visitIntegerLiteral(self)

    def dump(self, prefix:str) -> None:
        print(prefix+str(self.value))

##实数字面量
class DecimalLiteral(Expression):
    def __init__(self, value:int):
        super()
        self.value = value

    def accept(self, visitor) -> any:
        return visitor.visitDecimalLiteral(self)

    def dump(self, prefix:str) -> None:
        print(prefix+str(self.value))

##null字面量
class NullLiteral(Expression):
    def __init__(self):
        super()
        self.value = None

    def accept(self, visitor) -> any:
        return visitor.visitNullLiteral(self)

    def dump(self, prefix:str) -> None:
        print(prefix+self.value)

##boolean字面量
class BooleanLiteral(Expression):
    def __init__(self, value:bool):
        super()
        self.value = value

    def accept(self, visitor) -> any:
        return visitor.visitBooleanLiteral(self)

    def dump(self, prefix:str) -> None:
        print(prefix+self.value)

##基类，对AST做遍历，定义缺省的遍历方式，子类可重写
class AstVisitor:
    def visit(self, node:AstNode) -> any:
        return node.accept(self)

    def visitProg(self, prog:Prog) -> any:
        assert type(prog) is Prog
        retVal:any = None
        for x in prog.stmts:
            retVal = self.visit(x)
        return retVal

    def visitVariableDecl(self, variableDecl:VariableDecl) -> any:
        assert type(variableDecl) is VariableDecl
        if variableDecl.init is not None:
            return self.visit(variableDecl.init)

    def visitFunctionDecl(self, functionDecl:FunctionDecl) -> any:
        assert type(functionDecl) is FunctionDecl
        return self.visitBlock(functionDecl.body)

    def visitBlock(self, Block:Block) -> any:
        assert type(Block) is Block
        retVal:any = None
        for x in Block.stmts:
            retVal = self.visit(x)
        return retVal

    def visitExpressionStatement(self, stmt:ExpressionStatement) -> any:
        assert type(stmt) is ExpressionStatement
        return self.visit(stmt.exp)

    def visitBinary(self, exp:Binary) -> any:
        assert type(exp) is Binary
        self.visit(exp.exp1)
        self.visit(exp.exp2)

    def visitIntegerLiteral(self, exp:IntegerLiteral) -> any:
        assert type(exp) is IntegerLiteral
        return exp.value

    def visitDecimalLiteral(self, exp:DecimalLiteral) -> any:
        assert type(exp) is DecimalLiteral
        return exp.value

    def visitStringLiteral(self, exp:StringLiteral) -> any:
        assert type(exp) is StringLiteral
        return exp.value

    def visitNullLiteral(self, exp:NullLiteral) -> any:
        assert type(exp) is NullLiteral
        return exp.value

    def visitBooleanLiteral(self, exp:BooleanLiteral) -> any:
        assert type(exp) is BooleanLiteral
        return exp.value

    def visitVariable(self, variable:Variable) -> any:
        assert type(variable) is Variable
        return "undefined"

    def visitFunctionCall(self, functionCall:FunctionCall) -> any:
        assert type(functionCall) is FunctionCall
        return "undefined"

