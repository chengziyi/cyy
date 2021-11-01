'''
 * 1.条件语句和循环语句
 * 2.块作用域
'''

from scanner import Token, TokenKind, Scanner, CharStream, Op
from ast import AstVisitor, AstNode, Block, Prog, VariableDecl, FunctionDecl, FunctionCall, Statement, Expression, ExpressionStatement, Binary, Unary, IntegerLiteral, DecimalLiteral, StringLiteral, Variable, ReturnStatement, IfStatement, ForStatement, AstDumper
from parser import Parser
from semantic import SemanticAnalyer
from symbol import Symbol, SymKind, VarSymbol, FUN_println
from scope import Scope, ScopeDumper
import sys
from typing import Dict,List
import time

class Date:
    def __init__(self):
        self.__t=time.time()
    def getTime(self):
        return self.__t

#栈桢
#每个函数对应一级栈桢.
class StackFrame:
    def __init__(self):
        #存储变量的值
        self.values:Dict[Symbol, any] = dict()
        #返回值，当调用函数的时候，返回值放在这里
        self.retVal:any = None

##解释器
##遍历AST，执行函数调用
class Intepretor(AstVisitor):

    def __pushFrame(self, frame:StackFrame):
        self.callStack.append(frame);
        self.currentFrame = frame;

    def __popFrame(self):
        if self.callStack.__len__()>1:
            frame = self.callStack[self.callStack.__len__()-2];
            self.callStack.pop();
            self.currentFrame = frame;

    def __init__(self):
        super();
        self.callStack:List[StackFrame]=[]
        #创建顶层的栈桢
        self.currentFrame = StackFrame();
        self.callStack.append(self.currentFrame);

    ##函数声明不做任何事情
    def visitFunctionDecl(self, functionDecl:FunctionDecl, additional=None)->any:
        pass

    #遍历一个块
    def visitBlock(self, block:Block, additional=None):
        retVal:any=None
        for x in block.stmts:
            retVal = self.visit(x);
            #如果当前执行了一个返回语句，那么就直接返回，不再执行后面的语句。
            #如果存在上一级Block，也是中断执行，直接返回。
            if type(retVal) is not type(None) and ReturnValue.isReturnValue(retVal):
                return retVal;

        return retVal;

    #处理Return语句时，要把返回值封装成一个特殊的对象，用于中断后续程序的执行。
    def visitReturnStatement(self, returnStatement: ReturnStatement):
        retVal:any=None
        if returnStatement.exp is not None:
            retVal = self.visit(returnStatement.exp)
            self.__setReturnValue(retVal)

        return ReturnValue(retVal)#这里是传递一个信号，让Block和for循环等停止执行。

    #把返回值设置到上一级栈桢中（也就是调用者的栈桢）
    def __setReturnValue(self, retVal:any):
        frame = self.callStack[self.callStack.__len__()-2];
        frame.retVal = retVal

    #执行if语句
    def visitIfStatement(self, ifStmt:IfStatement):
        #计算条件
        conditionValue = self.visit(ifStmt.condition);
        #条件为真，则执行then部分
        if conditionValue:
            return self.visit(ifStmt.stmt);
        #条件为false，则执行else部分
        elif ifStmt.elseStmt is not None: 
            return self.visit(ifStmt.elseStmt)

    #执行for语句
    def visitForStatement(self, forStmt:ForStatement):
        #执行init
        if forStmt.init is not None:
            self.visit(forStmt.init)

        #计算循环结束的条件
        notTerminate = True if forStmt.condition is None else self.visit(forStmt.condition);
        while notTerminate:
            #执行循环体
            retVal = self.visit(forStmt.stmt);
            #处理循环体中的Return语句
            if type(retVal) is not None and ReturnValue.isReturnValue(retVal):
                return retVal

            #执行增量表达式
            if forStmt.increment is not None:
                self.visit(forStmt.increment)

            #执行循环判断
            notTerminate = True if forStmt.condition is None else self.visit(forStmt.condition)

    #运行函数调用
    #根据函数定义，执行函数体
    def visitFunctionCall(self, functionCall:FunctionCall, additional=None)->any:
        assert type(functionCall) is FunctionCall
        if functionCall.name == "println": #内置函数
            return self.__println(functionCall.arguments)
        elif functionCall.name == "tick":
            return self.__tick()
        elif functionCall.name == "integer_to_string":
            return self.__integer_to_string(functionCall.arguments)

        if functionCall.sym is not None:
            #清空返回值
            self.currentFrame.retVal = None

            #1.创建新栈桢
            frame = StackFrame()
            #2.计算参数值，并保存到新创建的栈桢
            functionDecl = functionCall.sym.decl
            if functionDecl.callSignature.paramList is not None:
                params = functionDecl.callSignature.paramList.params;
                for i in range(params.__len__()):
                    variableDecl = params[i]
                    val = self.visit(functionCall.arguments[i])
                    frame.values[variableDecl.sym] = val#设置到新的frame里

            #3.把新栈桢入栈 
            self.__pushFrame(frame)

            #4.执行函数
            self.visit(functionDecl.body)

            #5.弹出当前的栈桢
            self.__popFrame()

            #5.函数的返回值
            return self.currentFrame.retVal
        else:
            print("Runtime error, cannot find declaration of " + functionCall.name +".");
            return

    #内置函数println
    def __println(self, args: List[Expression]):
        if args.__len__()>0:
            retVal = self.visit(args[0])
            print(retVal)
        else:
            print()

        return 0

    #内置函数tick
    def __tick(self)->int:
        date = Date()
        value = Date.UTC(date.getFullYear(), date.getMonth(), date.getDate(), date.getHours(), date.getMinutes(), date.getSeconds(), date.getMilliseconds());
        return value

    #把整型转成字符串
    def __integer_to_string(self, args:List[Expression])->str:
        if args.__len__()>0:
            arg = self.visit(args[0])
            return arg.toString()

        return ""

    #变量声明
    #如果存在变量初始化部分，要存下变量值
    def visitVariableDecl(self, variableDecl:VariableDecl):
        if variableDecl.init is not None:
            v = self.visit(variableDecl.init)
            self.__setVariableValue(variableDecl.sym, v)
            return v

    #获取变量的值。
    #左值的情况，返回符号。否则，返回值。
    def visitVariable(self, v:Variable):
        if v.isLeftValue:
            return v.sym
        else:
            return self.__getVariableValue(v.sym)

    def __getVariableValue(self, sym:VarSymbol):
        return self.currentFrame.values.get(sym)

    def __setVariableValue(self, sym:VarSymbol, value:any):
        self.currentFrame.values[sym] = value

    def visitBinary(self, bi:Binary)->any:
        # print("visitBinary:"+bi.op)
        ret:any = None
        v1 = self.visit(bi.exp1)
        v2 = self.visit(bi.exp2)

        if bi.op == Op.Plus:
            ret = v1 + v2
        elif bi.op == Op.Minus:
            ret = v1 - v2
        elif bi.op == Op.Multiply:
            ret = v1 * v2
        elif bi.op == Op.Divide:
            ret = v1 / v2
        elif bi.op == Op.Modulus:
            ret = v1 % v2
        elif bi.op == Op.G:
            ret = v1 > v2
        elif bi.op == Op.GE:
            ret = v1 >= v2
        elif bi.op == Op.L:
            ret = v1 < v2
        elif bi.op == Op.LE:
            ret = v1 <= v2
        elif bi.op == Op.EQ:
            ret = v1 == v2
        elif bi.op == Op.NE:
            ret = v1 != v2
        elif bi.op == Op.And:
            ret = v1 and v2
        elif bi.op == Op.Or:
            ret = v1 or v2
        elif bi.op == Op.Assign:
            varSymbol = v1
            self.__setVariableValue(varSymbol, v2)
        else:
            print("Unsupported binary operation: "+Op[bi.op])

        return ret

    #计算一元表达式
    def visitUnary(self, u:Unary):
        v = self.visit(u.exp)
        varSymbol:VarSymbol=None
        value:any=None
        
        if u.op == Op.Inc:#'++'
            varSymbol = v
            value = self.__getVariableValue(varSymbol)
            self.__setVariableValue(varSymbol, value+1)
            if u.isPrefix:
                return value+1
            else:
                return value
        elif u.op == Op.Dec: #'--'
            varSymbol = v
            value = self.__getVariableValue(varSymbol);
            self.__setVariableValue(varSymbol, value-1);
            if u.isPrefix:
                return value-1
            else:
                return value
        elif u.op == Op.Plus: #'+'
            return v #不需要做任何动作
        elif u.op == Op.Minus: #'-'
            return -v #对值取反   
        else:
            print("Unsupported unary op: " + Op[u.op])

#用于封装Return语句的返回结果，并结束后续语句的执行
class ReturnValue:
    def __init__(self, value:any):
        self.value = value
        self.tag_ReturnValue = 0

    @staticmethod
    def isReturnValue(v:any):
        return type(v) is type(ReturnValue)

##主程序
def compileAndRun(program:str):
    print("源代码:")
    print(program)

    ##词法分析
    print("\n词法分析结果:")
    ##打印词法分析的结果
    scanner = Scanner(CharStream(program))
    while scanner.peek().kind != TokenKind.EOF:
        print(scanner.next().toString())
    ##回到开头 进行语法分析
    scanner = Scanner(CharStream(program))

    ##语法分析
    parser = Parser(scanner)
    prog = parser.parseProg()
    print("\nAST before RefResolve:")
    astDumper = AstDumper()
    astDumper.visit(prog, "")

    ##语义分析
    semanticAnalyer = SemanticAnalyer()
    semanticAnalyer.execute(prog)
    print("\n符号表:")
    ScopeDumper().visit(prog, "")
    print("\n语义分析后的AST，注意变量和函数已被消解:")
    astDumper.visit(prog,"")

    if parser.errors.__len__() > 0 or semanticAnalyer.errors.__len__()>0:
        print("\n共发现" + str(parser.errors.__len__()) + "个语法错误，" + str(semanticAnalyer.errors.__len__()) + "个语义错误。");
        return

    #运行程序
    print("\n通过AST解释器运行程序:");
    date1 = Date()
    retVal = Intepretor().visit(prog)
    date2 = Date()
    print("程序返回值：")
    print(retVal)
    print("耗时："+ str(date2.getTime()-date1.getTime()) + "秒")

if __name__ == "__main__":
    if len(sys.argv)<2:
        print("Usage: python "+sys.argv[0]+' FILENAME')
        quit()

    filename = sys.argv[1]
    with open(filename, 'r') as f:
        data = f.readlines()
    data = ''.join(data)
    compileAndRun(data)



