'''
 * 作用域
 * @version 0.3
 * @author czy
 * @license MIT
'''

from symbol import Symbol, SymKind, SymbolDumper
from ast import AstNode, AstVisitor, Block, FunctionDecl, ForStatement

#作用域，限定标识符的可见性
class Scope:
    def __init__(self, enclosingScope):
        '''
        enclosingScope:Scope or None
        '''
        #上级作用域
        self.enclosingScope = enclosingScope
        #名称作为key
        self.name2sym:Dict[str,Symbol] = dict()

    def enter(self, name:str, sym:Symbol) -> None:
        '''
        把符号记入符号表(作用域)
        '''
        self.name2sym[name] = sym

    def hasSymbol(self, name:str) -> bool:
        '''
        查询是否有某名称的符号
        '''
        return name in self.name2sym

    def getSymbol(self, name:str) -> Symbol or None:
        sym = self.name2sym.get(name)
        if sym is not None:
            return sym
        else:
            return None

    def getSymbolCascade(self, name:str) -> Symbol or None:
        '''
        级联查找某个符号，先从本作用域查找，查不到去上一级作用域
        '''
        sym = self.getSymbol(name)
        if sym is not None:
            return sym
        elif self.enclosingScope is not None:
            return self.enclosingScope.getSymbolCascade(name)
        else:
            return None

#打印Scope信息
class ScopeDumper(AstVisitor):
    def visitFunctionDecl(self, functionDecl:FunctionDecl, prefix:any)->any:
        print(prefix + "Scope of function: " + functionDecl.name)

        ##显示本级Scope
        if functionDecl.scope is not None:
            self.__dumpScope(functionDecl.scope, prefix)
        else:
            print(prefix + "{None}")

        ##继续遍历
        super().visitFunctionDecl(functionDecl, prefix+"    ")

    def visitBlock(self, block:Block, prefix:any)->any:
        print(prefix + "Scope of block")

        #显示本级Scope
        if block.scope is not None:
            self.__dumpScope(block.scope, prefix)
        else:
            print(prefix+"{None}")

        #继续遍历
        super().visitBlock(block, prefix+"    ")

    def visitForStatement(self, stmt:ForStatement, prefix:any) -> any:
        print(prefix+"Scope of for statement")
        if stmt.scope is not None:
            self.__dumpScope(stmt.scope, prefix)
        else:
            print(prefix + "{None}")

        super().visitForStatement(stmt, prefix)

    def __dumpScope(self, scope:Scope, prefix:str):
        if scope.name2sym.__len__() > 0:
            #遍历该作用域的符号
            symbolDumper = SymbolDumper()
            for sym in scope.name2sym.values():
                symbolDumper.visit(sym, prefix+"    ")
        else:
            print(prefix + "    {empty}")


