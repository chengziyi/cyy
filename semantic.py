'''
 * 语义分析
 * @version 0.2
 * @author 程梓益
 * @license MIT License
 * 
 * 当前特性：
 * 1.简单的符号表
 * 2.简单的函数消解
 * 3.简单的变量消解
 * 
'''

from ast import AstVisitor, AstNode, Block, Prog, Decl,\
VariableDecl, FunctionDecl, FunctionCall, Statement,\
Expression, ExpressionStatement, Binary, IntegerLiteral,\
DecimalLiteral, StringLiteral, Variable
from typing import Dict
from enum import Enum, unique

##符号类型
@unique
class SymKind(Enum):
    Variable = 1
    Function = 2
    Class = 3
    Interface = 4

##符号表条目
class Symbol:
    def __init__(self, name:str, decl:Decl, kind:SymKind):
        self.name = name
        self.decl = decl
        self.kind = kind

##符号表 保存变量、函数、类等的名称和它的类型，声明的位置(AST节点)
class SymTable:
    def __init__(self):
        self.table:Dict = dict()

    def enter(self, name:str, decl:Decl, symType:SymKind)->None:
        self.table[name] = Symbol(name, decl, symType)

    def hasSymbol(self, name:str) -> bool:
        return name in self.table

    def getSymbol(self, name:str) -> Symbol or None:
        '''
        根据名称查找符号
        name 符号名称
        return symbol，没有返回None
        '''
        item = self.table.get(name)
        if item is not None:
            return item
        else:
            return None

##遍历AST 建立符号表 把符号加入符号表
class Enter(AstVisitor):
    def __init__(self, symTable:SymTable):
        super()
        self.symTable = symTable

    ##把函数声明加入符号表
    def visitFunctionDecl(self, functionDecl:FunctionDecl) -> any:
        if self.symTable.hasSymbol(functionDecl.name):
            print("Dumplicate symbol: "+functionDecl.name)
        self.symTable.enter(functionDecl.name, functionDecl, SymKind.Function)

    ##把变量声明加入符号表
    def visitVariableDecl(self, variableDecl:VariableDecl) -> any:
        if self.symTable.hasSymbol(variableDecl.name):
            print("Dumplicate symbol: "+variableDecl.name)
        self.symTable.enter(variableDecl.name, variableDecl, SymKind.Variable)

##引用消解 函数引用消解 变量引用消解
##遍历AST 如果发现函数调用和变量引用，就去找它的定义
class RefResolver(AstVisitor):
    def __init__(self, symTable:SymTable):
        super()
        self.symTable = symTable

    ##函数引用
    def visitFunctionCall(self, functionCall:FunctionCall)->any:
        assert type(functionCall) is FunctionCall
        symbol = self.symTable.getSymbol(functionCall.name)
        if symbol is not None and symbol.kind == SymKind.Function:
            assert type(symbol.decl) is FunctionDecl
            functionCall.decl = symbol.decl
        else:
            if functionCall.name != "println":
                print("Error: cannot find declaration of function "+functionCall.name)

    ##变量引用
    def visitVariable(self, variable:Variable)->any:
        assert type(variable) is Variable
        symbol = self.symTable.getSymbol(variable.name)
        if symbol is not None and symbol.kind == SymKind.Variable:
            assert type(symbol.decl) is VariableDecl
            variable.decl = symbol.decl
        else:
            print("Error: cannot find declaration of variable "+variable.name)









