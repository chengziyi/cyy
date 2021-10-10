'''
语法分析 变量声明和变量赋值
符号表
变量引用消解
解释器
左值
'''

from scanner import Token, TokenKind, Scanner, CharStream
from ast import AstVisitor, AstNode, Block, Prog,\
VariableDecl, FunctionDecl, FunctionCall, Statement,\
Expression, ExpressionStatement, Binary, IntegerLiteral,\
DecimalLiteral, StringLiteral, Variable
from parser import Parser
from semantic import SymTable, SymKind, Enter, RefResolver
import sys
from typing import Dict

##左值
class LeftValue:
    def __init__(self, variable:Variable):
        self.variable = variable

##解释器
##遍历AST，执行函数调用
class Interpretor(AstVisitor):
    def __init__(self):
        ##存储变量值
        self.values:Dict = dict()

    ##函数声明不做任何事情
    def visitFunctionDecl(self, functionDecl:FunctionDecl)->any:
        pass

    #运行函数调用
    #根据函数定义，执行函数体
    def visitFunctionCall(self, functionCall:FunctionCall)->any:
        assert type(functionCall) is FunctionCall
        print("running function:"+functionCall.name)
        if functionCall.name == 'println':
            if len(functionCall.parameters)>0:
                retVal = self.visit(functionCall.parameters[0])
                if self.__isLeftValue(retVal):
                    retVal = self.__getVariableValue(retVal.variable.name)
                print(retVal)
            else:
                print()
            return 0
        else:
            if functionCall.decl is not None:
                self.visitBlock(functionCall.decl.body)

    ##变量声明 如果存在初始化部分，要存下变量值
    def visitVariableDecl(self, variableDecl:VariableDecl)->any:
        if variableDecl.init is not None:
            v = self.visit(variableDecl.init)
            if self.__isLeftValue(v):
                v = self.__getVariableValue(v.variable.name)
            self.__setVariableValue(variableDecl.name, v)
            return v

    ##获取变量的值 左值既可以赋值，又可以获取当前值
    def visitVariable(self, v:Variable) -> any:
        return LeftValue(v)

    def __getVariableValue(self, varName:str) -> any:
        assert type(varName) is str
        return self.values.get(varName)

    def __setVariableValue(self, varName:str, value:any)->any:
        self.values[varName] = value

    def __isLeftValue(self, v:any) -> bool:
        return type(v) is LeftValue and v.variable is not None

    def visitBinary(self, bi:Binary)->any:
        print("visitBinary:"+bi.op)
        ret:any = None
        v1 = self.visit(bi.exp1)
        v2 = self.visit(bi.exp2)
        v1left:LeftValue or None = None
        v2left:LeftValue or None = None
        if self.__isLeftValue(v1):
            v1left = v1
            v1 = self.__getVariableValue(v1left.variable.name)
            print("value of "+v1left.variable.name+" : ",v1)
        if self.__isLeftValue(v2):
            v2left = v2
            v2 = self.__getVariableValue(v2left.variable.name)
        if bi.op == '+':
            ret = v1 + v2
        elif bi.op == '-':
            ret = v1 - v2
        elif bi.op == '*':
            ret = v1 * v2
        elif bi.op == '/':
            ret = v1 / v2
        elif bi.op == '%':
            ret = v1 % v2
        elif bi.op == '>':
            ret = v1 > v2
        elif bi.op == '>=':
            ret = v1 >= v2
        elif bi.op == '<':
            ret = v1 < v2
        elif bi.op == '<=':
            ret = v1 <= v2
        elif bi.op == '&&':
            ret = v1 and v2
        elif bi.op == '||':
            ret = v1 or v2
        elif bi.op == '=':
            if v1left is not None:
                self.__setVariableValue(v1left.variable.name, v2)
            else:
                print("Assignment need a left value")
        else:
            print("Unsupported binary operation: "+bi.op)

        return ret

##主程序
def compileAndRun(program:str):
    print("源代码:")
    print(program)

    ##词法分析
    print("\n词法分析结果:")
    ##打印词法分析的结果
    tokenizer = Scanner(CharStream(program))
    while tokenizer.peek().kind != TokenKind.EOF:
        t=tokenizer.next()
        print(t.kind, '\t', t.text)
    ##回到开头 进行语法分析
    tokenizer = Scanner(CharStream(program))

    ##语法分析
    prog:Prog = Parser(tokenizer).parseProg()
    print("\nAST before RefResolve:")
    prog.dump("")

    ##语义分析
    symTable = SymTable()
    Enter(symTable).visit(prog) ##建立符号表
    RefResolver(symTable).visit(prog) ##引用消解
    print("\nAST RefResolved:")
    prog.dump("")

    print("\nrunning program:")
    retVal = Interpretor().visit(prog)
    print(f"return: {retVal}")

if __name__ == "__main__":
    if len(sys.argv)<2:
        print("Usage: python "+sys.argv[0]+' FILENAME')
        quit()

    filename = sys.argv[1]
    with open(filename, 'r') as f:
        data = f.readlines()
    data = ''.join(data)
    compileAndRun(data)



