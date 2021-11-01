'''
符号表和作用域
version 0.5
author czy
license MIT
'''
from typing import List, Dict
from cyy_types import SysTypes, Type, FunctionType
from enum import Enum, unique

##符号表

##符号类型
@unique
class SymKind(Enum):
    Variable = 1
    Function = 2
    Class = 3
    Interface = 4

##visitor
class SymbolVisitor:
    def visitVarSymbol(self, sym, additional:any) -> any:
        pass
    def visitFunctionSymbol(self, sym, additional:any) -> any:
        pass

##符号
class Symbol:
    def __init__(self, name:str, theType:Type, kind:SymKind):
        self.name = name
        self.theType = theType
        self.kind = kind

    def accept(self, vistor:SymbolVisitor, additional:any) -> any:
        '''
        visitor
        param: vistor
        param: additional 额外需要传递给visitor的信息
        '''
        pass

class VarSymbol(Symbol):
    def __init__(self, name:str, theType:Type):
        super().__init__(name, theType, SymKind.Variable)
        self.theType = theType

    def accept(self, vistor:SymbolVisitor, additional:any) -> any:
        vistor.visitVarSymbol(self, additional)

class FunctionSymbol(Symbol):
    def __init__(self, name:str, theType:FunctionType, vars:List[VarSymbol]=[]):
        super().__init__(name, theType, SymKind.Function)
        self.theType = theType
        self.vars = vars
        self.byteCode:List[int] or None = None #存放生成的字节码
        self.opStackSize:int = 10 #操作数栈的大小
        self.decl:FunctionDecl or None = None #存放AST,作为代码来运行

    def accept(self, vistor:SymbolVisitor, additional:any) -> any:
        vistor.visitFunctionSymbol(self, additional)


    def getNumParams(self) -> int:
        return self.theType.paramTypes.length

class SymbolDumper(SymbolVisitor):
    def visit(self, sym:Symbol, additional:any):
        return sym.accept(self, additional)

    def visitVarSymbol(self, sym:VarSymbol, additional:any) -> any:
        '''
        输出VarSymbol调试信息
        '''
        print(additional + sym.name + "{" + SymKind[sym.kind] + "}")

    def visitFunctionSymbol(self, sym:FunctionSymbol, additional:any) -> any:
        '''
        输出FunctionSymbol调试信息
        '''

        print(additional + sym.name + "{" + sym.kind.name + ", local var count:"+str(sym.vars.__len__()) + "}")
        if (sym.byteCode is not None):
            string:str = ''
            for code in sym.byteCode:
                string += str(code) + " "
            print(additional + "    bytecode: " + string)

##系统内置符号
FUN_println = FunctionSymbol(name="println", theType=FunctionType(returnType=SysTypes.Void, paramTypes=[SysTypes.String]), vars=list([VarSymbol(name="a", theType=SysTypes.String)]))
FUN_tick = FunctionSymbol(name="tick", theType=FunctionType(SysTypes.Integer,[]), vars=[])
FUN_integer_to_string = FunctionSymbol(name="integer_to_string", theType=FunctionType(SysTypes.String,[SysTypes.Integer]), vars=[VarSymbol("a",SysTypes.Integer)])

built_ins:Dict[str,FunctionSymbol] = {"println":FUN_println,"tick":FUN_tick,"integer_to_string":FUN_integer_to_string}

FUN_string_create_by_str = FunctionSymbol("string_create_by_str",FunctionType(SysTypes.String,[SysTypes.String]),[VarSymbol("a",SysTypes.String)])
FUN_string_concat = FunctionSymbol("string_concat",FunctionType(SysTypes.String,[SysTypes.String,SysTypes.String]),[VarSymbol("str1",SysTypes.String),VarSymbol("str2",SysTypes.String)])

intrinsics:Dict[str,FunctionSymbol] = {"string_create_by_str":FUN_string_create_by_str,"string_concat":FUN_string_concat}



