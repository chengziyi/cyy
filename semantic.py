'''
 * 语义分析
 * @version 0.2
 * @author 程梓益
 * @license MIT License
 * 
 * 当前特性：
 * 1.树状的符号表
 * 2.简单的引用消解：没有考虑声明的先后顺序，也没有考虑闭包
 * 3.简单的作用域
 * 
'''

from ast import AstVisitor, AstNode, Block, Prog, Decl, VariableDecl, FunctionDecl, FunctionCall, Statement, Expression, ExpressionStatement, Binary, IntegerLiteral, DecimalLiteral, StringLiteral, Variable, ReturnStatement, Unary, CallSignature, BooleanLiteral, NullLiteral
from symbol import Symbol, SymKind, FunctionSymbol, VarSymbol, built_ins
from scope import Scope
from cyy_types import SysTypes, Type, FunctionType
from scanner import Op, Operators
from error import CompilerError
from typing import Dict
from enum import Enum, unique

class SemanticAnalyer:
    def __init__(self):
        self.passes:List[SemanticAstVisitor]=[Enter(),RefResolver(),TypeChecker(),TypeConverter(),LeftValueAttributor()]
        self.errors:List[CompilerError]=[]
        self.warnings:List[CompilerError]=[]

    def execute(self, prog:Prog):
        self.errors=[]
        self.warnings=[]
        for pas in self.passes:
            pas.visitProg(prog)
            self.errors.extend(pas.errors)
            self.warnings.append(pas.warnings)

class SemanticError(CompilerError):
    def __init__(self, msg:str, node:AstNode, isWarning=False):
        super(msg, node.beginPos, isWarning)
        self.node = node

class SemanticAstVisitor(AstVisitor):
    def __init__(self):
        self.errors:List[CompilerError]=[]
        self.warnings:List[CompilerError]=[]

    def addError(self, msg:str, node:AstNode):

        self.errors.append(SemanticError(msg, node))
        print("@"+node.beginPos.toString()+" : "+msg)

    def addWarning(self, msg:str, node:AstNode):
        self.warnings.append(SemanticError(msg, node, True))
        print("@"+node.beginPos.toString()+" : "+msg)

##建立符号表 把符号加入符号表
class Enter(SemanticAstVisitor):
    def __init__(self):
        super(Enter, self).__init__()
        self.scope:Scope or None = None#当前所属scope
        self.functionSym:FunctionSymbol or None=None

    def visitProg(self, prog:Prog):
        '''
        返回最顶级的scope对象
        '''
        sym = FunctionSymbol("main", FunctionType(SysTypes.Integer, []))
        prog.sym = sym
        self.functionSym = sym

        return super().visitProg(prog)

    def visitFunctionDecl(self, functionDecl:FunctionDecl, additional:any)->any:
        currentScope = self.scope

        #创建函数的symbol
        paramTypes:List[Type]=[]
        if functionDecl.callSignature.paramList is not None:
            for p in functionDecl.callSignature.paramList.params:
                paramTypes.append(p.theType)
        sym = FunctionSymbol(functionDecl.name, FunctionType(functionDecl.callSignature.theType, paramTypes))
        sym.decl = functionDecl
        functionDecl.sym = sym

        ##把函数加入当前scope
        if currentScope.hasSymbol(functionDecl.name):
            self.addError("Dumplicate symbol:"+functionDecl.name, functionDecl)
        else:
            currentScope.enter(functionDecl.name, sym)

        #修改当前函数符号
        lastFunctionSym = self.functionSym
        self.functionSym = sym

        #创建新的scope, 用来存放参数
        oldScope = currentScope
        self.scope = Scope(oldScope)
        functionDecl.scope = self.scope

        ##遍历子节点
        super().visitFunctionDecl(functionDecl,additional)

        ##恢复当前函数
        self.functionSym = lastFunctionSym

        ##恢复原来的scope
        self.scope = oldScope

    def visitBlock(self, block:Block, additional:any = None)->any:
        '''
        遇到块的时候，建立一级新的作用域，支持块作用域
        '''
        # assert type(block) is Block,f"type of block is {type(block)}"
        oldScope = self.scope
        self.scope = Scope(self.scope)
        block.scope = self.scope

        ##调用父类方法，遍历所有语句
        super().visitBlock(block)

        ##重新设置当前scope
        self.scope = oldScope

    def visitVariableDecl(self, variableDecl:VariableDecl)->any:
        '''
        把变量声明加入符号表
        '''
        currentScope = self.scope
        if currentScope.hasSymbol(variableDecl.name):
            self.addError("Dumplicate symbol:"+variableDecl.name, variableDecl)
        ##把变量加入当前符号表
        sym = VarSymbol(variableDecl.name, variableDecl.theType)
        variableDecl.sym = sym
        currentScope.enter(variableDecl.name, sym)

        ##把本地变量加入函数符号中，可用于后面生成代码
        self.functionSym.vars.append(sym)

##引用消解 函数引用消解 变量引用消解
##遍历AST 如果发现函数调用和变量引用，就去找它的定义
class RefResolver(SemanticAstVisitor):
    def __init__(self):
        super(RefResolver, self).__init__()
        self.scope:Scope or None = None
        #记录每个scope已声明变量的列表
        self.declaredVarsMap:Dict[Scope,Dict[str,VarSymbol]]=dict()

    def visitFunctionDecl(self, functionDecl:FunctionDecl, additional=None)->any:

        ##修改scope
        oldScope=self.scope
        self.scope=functionDecl.scope
        assert self.scope is not None, "scope can't be none"

        #为已申明变量设置一个存储区域
        self.declaredVarsMap[self.scope]=dict()

        #遍历下级节点
        super().visitFunctionDecl(functionDecl)

        #还原scope
        self.scope=oldScope

    def visitBlock(self, block:Block, additional=None)->any:
        #修改当前scope
        oldScope=self.scope
        self.scope=block.scope
        assert self.scope is not None,"scope can't be None"

        #为声明变量设置存储区
        self.declaredVarsMap[self.scope]=dict()

        #遍历下级节点
        super().visitBlock(block)

        #重新设置scope
        self.scope=oldScope

    ##函数消解，函数不需要声明在前，使用在后
    def visitFunctionCall(self, functionCall:FunctionCall, additional=None)->any:
        assert type(functionCall) is FunctionCall
        currentScope=self.scope
        ##内置函数
        if functionCall.name in built_ins:
            functionCall.sym = built_ins.get(functionCall.name)
        else:
            functionCall.sym = currentScope.getSymbolCascade(functionCall.name)
        #调用下级
        super().visitFunctionCall(functionCall)

    ##标记变量是否已被声明
    def visitVariableDecl(self, variableDecl:VariableDecl)->any:
        assert type(variable) is Variable
        currentScope = self.scope
        declaredSyms = self.declaredVarsMap.get(currentScope)
        sym = currentScope.getSymbol(variableDecl.name)
        #需要检查sym是否是变量
        if sym is not None:
            declaredSyms[variableDecl.name] = sym

        super.visitVariableDecl(variableDecl)

    def visitVariable(self, variable:Variable)->any:
        '''
        变量引用消解，变量必须声明在前 使用在后
        '''
        currentScope = self.scope
        variable.sym = self.__findVariableCascade(currentScope, variable)

    def __findVariableCascade(self, scope:Scope, variable:Variable)->VarSymbol or None:
        '''
        逐级查找某个符号是不是在声明前就使用了
        '''
        declaredSyms = self.declaredVarsMap.get(scope)
        symInScope = scope.getSymbol(variable.name)
        if symInScope is not None:
            if variable.name in declaredSyms:
                #找到，成功返回
                return declaredSyms.get(variable.name)
            else:
                if symInScope.kind == SymKind.Variable:
                    self.addError("Variable: '"+variable.name+"' is used before declaration")
                else:
                    self.addError("We expect a variable of name: '"+variable.name+"', but find a "+SymKind[symInScope.kind]+'.', variable)
        else:
            if scope.enclosingScope is not None:
                return self.__findVariableCascade(scope.enclosingScope, variable)
            else:
                self.addError("Cannot find a variable of name: '"+variable.name+"'", variable)
        return None

##属性分析
##类型计算和检查
class LeftValueAttributor(SemanticAstVisitor):
    def __init__(self):
        super().__init__()
        self.parentOperator:Op or None = None

    def visitBinary(self, binary:Binary):
        '''
        检查赋值符号和.符号左边是否是左值
        '''
        if Operators.isAssignOp(binary.op) or binary.op == Op.Dot:
            lastParentOperator = self.parentOperator
            self.parentOperator = binary.op

            #检查左子节点
            self.visit(binary.exp1)
            if not binary.exp1.isLeftValue:
                self.addError("Left child of operator "+Op[binary.op]+" need a left value", binary.exp1)
            ##恢复原来的状态信息
            self.parentOperator = lastParentOperator

            ##继续遍历右子节点
            self.visit(binary.exp2)
        else:
            super.visitBinary(binary)

    def visitUnary(self, u:Unary):
        if u.op == Op.Inc or u.op == Op.Dec:
            lastParentOperator = self.parentOperator
            self.parentOperator = u.op

            self.visit(u.exp)
            if not u.exp.isLeftValue:
                self.addError("Unary operator "+Op[u.op]+"can only be applied to a left value",u)
            #恢复原来的状态信息
            self.parentOperator = lastParentOperator
        else:
            super.visitUnary(u)

    def visitVariable(self, v:Variable):
        '''
        变量都可以作为左值，除非类型是void
        '''
        if self.parentOperator is not None:
            t = v.theType
            if not t.hasVoid():
                v.isLeftValue = True

    def visitFunctionCall(self, functionCall:FunctionCall, additional=None):
        '''
        函数调用是在.符号左边，并且返回值不为void的时候，可以作为左值
        '''
        if self.parentOperator == Op.Dot:
            functionType = functionCall.theType
            if not functionType.returnType.hasVoid():
                functionCall.isLeftValue = True

#类型检查
class TypeChecker(SemanticAstVisitor):

    def visitVariableDecl(self, variableDecl:VariableDecl)->any:
        super.visitVariableDecl(variableDecl)

        if variableDecl.init is not None:
            t1 = variableDecl.theType
            t2 = variableDecl.init.theType
            if not t2.LE(t1):
                self.addError("Operator '=' can not be applied to '"+t1.name+"' and '"+t2.name+"'." ,variableDecl)

            #类型推断：对于any类型，变成=号右边的具体类型
            if t1==SysTypes.Any:
                variableDecl.theType = t2;
                # variableDecl.inferredType = t2;
                #重点是把类型记入符号中，这样相应的变量声明就会获得准确的类型
                #由于肯定是声明在前，使用在后，所以变量引用的类型是准确的。
                variableDecl.sym.theType = t2;

    def visitBinary(self, bi:Binary)->any:
        super.visitBinary(bi)

        t1 = bi.exp1.theType
        t2 = bi.exp2.theType
        if Operators.isAssignOp(bi.op):
            bi.theType = t1
            if not t2.LE(t1):  #检查类型匹配
                self.addError("Operator '" + Op[bi.op] + "' can not be applied to '"+t1.name+"' and '"+t2.name+"'." ,bi)
        elif bi.op == Op.Plus: #有一边是string，或者两边都是number才行。
            if t1 == SysTypes.String or t2 == SysTypes.String:
                bi.theType = SysTypes.String
            elif t1.LE(SysTypes.Number) and t2.LE(SysTypes.Number):
                bi.theType = Type.getUpperBound(t1, t2);
            else:
                self.addError("Operator '" + Op[bi.op] + "' can not be applied to '"+t1.name+"' and '"+t2.name+"'." ,bi);
        elif Operators.isArithmeticOp(bi.op):
            if t1.LE(SysTypes.Number) and t2.LE(SysTypes.Number):
                bi.theType = Type.getUpperBound(t1, t2)
            else:
                self.addError("Operator '" + Op[bi.op] + "' can not be applied to '"+t1.name+"' and '"+t2.name+"'." ,bi);
        elif Operators.isRelationOp(bi.op):
            if t1.LE(SysTypes.Number) and t2.LE(SysTypes.Number):
                bi.theType = SysTypes.Boolean
            else:
                self.addError("Operator '" + Op[bi.op] + "' can not be applied to '"+t1.name+"' and '"+t2.name+"'." ,bi)
        elif Operators.isLogicalOp(bi.op):
            if t1.LE(SysTypes.Boolean) and t2.LE(SysTypes.Boolean):
                bi.theType = SysTypes.Boolean
            else:
                self.addError("Operator '" + Op[bi.op] + "' can not be applied to '"+t1.name+"' and '"+t2.name+"'." ,bi);
        else:
            self.addError("Unsupported binary operator: " + Op[bi.op],bi)

    def visitUnary(self, u:Unary)->any:
        super.visitUnary(u)

        t = u.exp.theType
        #要求必须是个左值
        if u.op == Op.Inc or u.op == Op.Dec:
            if t.LE(SysTypes.Number):
                u.theType = t
            else:
                self.addError("Unary operator " + Op[u.op] + "can not be applied to '"+t.name+"'." ,u)
        elif u.op == Op.Minus or u.op == Op.Plus:
            if t.LE(SysTypes.Number):
                u.theType = t
            else:
                self.addError("Unary operator " + Op[u.op] + "can not be applied to '"+t.name+"'." ,u)
        elif u.op == Op.Not:
            if t.LE(SysTypes.Boolean):
                u.theType = t
            else:
                self.addError("Unary operator " + Op[u.op] + "can not be applied to '"+t.name+"'." ,u)
        else:
            self.addError("Unsupported unary operator: " + Op[u.op] + " applied to '"+t.name+"'." ,u)

    #用符号的类型（也就是变量声明的类型），来标注本节点
    def visitVariable(self, v:Variable):
        if v.sym is not None:
            v.theType = v.sym.theType

    def visitFunctionCall(self, functionCall:FunctionCall, additional=None):
        if functionCall.sym is not None:
            functionType = functionCall.sym.theType

            #注意：不使用函数类型，而是使用返回值的类型
            functionCall.theType = functionType.returnType

            #检查参数数量
            if functionCall.arguments.__len__() != functionType.paramTypes.__len__():
                self.addError("FunctionCall of " + functionCall.name +" has " + functionCall.arguments.__len__() + " arguments, while expecting " + functionType.paramTypes.__len__() +".",functionCall)
        
            #检查参数的类型
            for i in range(functionCall.arguments.__len__()):
                self.visit(functionCall.arguments[i])
                if i < functionType.paramTypes.__len__():
                    t1 = functionCall.arguments[i].theType
                    t2 = functionType.paramTypes[i]

                    if not t1.LE(t2) and t2 != SysTypes.String:
                        self.addError("Argument " + i + " of FunctionCall " + functionCall.name + "is of Type " + t1.name + ", while expecting "+t2.name, functionCall);


#类型转换
#添加必要的AST节点，来完成转换
#目前特性：其他类型转换成字符串
class TypeConverter(SemanticAstVisitor):
    def visitBinary(self, bi:Binary):
        super.visitBinary(bi)

        t1 = bi.exp1.theType
        t2 = bi.exp2.theType

        if Operators.isAssignOp(bi.op):
            if t1 == SysTypes.String and t2 != SysTypes.String:
                if t2 == SysTypes.Integer:
                    exp = FunctionCall(bi.exp2.beginPos, bi.exp2.endPos,"integer_to_string",[bi.exp2])
                    exp.sym = built_ins.get("integer_to_string")
                    bi.exp2 = exp

        elif bi.op == Op.Plus: #有一边是string，或者两边都是number才行。
            if t1 == SysTypes.String or t2 == SysTypes.String:
                if t1 == SysTypes.Integer or t1 == SysTypes.Number:
                    exp = FunctionCall(bi.exp1.beginPos, bi.exp1.endPos,"integer_to_string",[bi.exp1])
                    exp.sym = built_ins.get("integer_to_string")
                    bi.exp1 = exp

                if t2 == SysTypes.Integer or t2 == SysTypes.Number:
                    exp = FunctionCall(bi.exp2.beginPos, bi.exp2.endPos,"integer_to_string",[bi.exp2]);
                    exp.sym = built_ins.get("integer_to_string")
                    bi.exp2 = exp

    def visitFunctionCall(self, functionCall:FunctionCall, additional=None):
        if functionCall.sym is not None:
            functionType = functionCall.sym.theType

            #看看参数有没有可以转换的。
            for i in range(functionCall.arguments.__len__()):
                self.visit(functionCall.arguments[i])
                if i < functionType.paramTypes.__len__():
                    t1 = functionCall.arguments[i].theType
                    t2 = functionType.paramTypes[i]
                    if (t1 == SysTypes.Integer or t1 == SysTypes.Number) and t2 == SysTypes.String:
                        exp = FunctionCall(functionCall.arguments[i].beginPos, functionCall.arguments[i].endPos,"integer_to_string",[functionCall.arguments[i]])
                        exp.sym = built_ins.get("integer_to_string")
                        functionCall.arguments[i] = exp


#常量折叠
class ConstFolder(SemanticAstVisitor):
    
    def visitBinary(self, bi:Binary):
        v1 = bi.exp1.constValue;
        v2 = bi.exp2.constValue;
        if Operators.isAssignOp(bi.op):
            if type(v2) is not type(None):
                if bi.op == Op.Assign: #暂时只支持=号
                    bi.exp1.constValue = v1
                    bi.constValue = v1
                else:
                    self.addError("Unsupported operator: " + Op[bi.op] +"in ConstFolder",bi);

        elif type(v1) is not type(None) and type(v2) is not type(None):
            v:any=None
            if bi.op == Op.Plus:
                v = v1 + v2
            elif bi.op == Op.Minus:
                v = v1-v2
            elif bi.op == Op.Multiply:
                v = v1*v2
            elif bi.op == Op.Divide:
                v = v1/v2
            elif bi.op == Op.Modulus:
                v = v1%v2
            elif bi.op == Op.G:
                v = v1>v2
            elif bi.op == Op.GE:
                v = v1>=v2
            elif bi.op == Op.L:
                v = v1<v2
            elif bi.op == Op.LE:
                v = v1<=v2
            elif bi.op == EQ:
                v = v1==v2
            elif bi.op == NE:
                v = v1!=v2
            elif bi.op == And:
                v = v1 and v2
            elif bi.op == Op.Or:
                v = v1 or v2
            else:
                self.addError("Unsupported binary operator: "+Op[bi.op]+"in ConstFolder", bi)

            bi.op = v

    def visitUnary(self, u:Unary):
        v1 = u.exp.constValue
        if type(v1) is not type(None):
            if u.op == Op.Inc:
                if u.isPrefix:
                    u.exp.constValue += 1
                    u.constValue = u.exp.constValue
                else:
                    u.constValue = v1;
                    u.exp.constValue += 1;

            elif u.op == Op.Dec:
                if u.isPrefix:
                    u.exp.constValue -= 1
                    u.constValue = u.exp.constValue
                else:
                    u.constValue = v1
                    u.exp.constValue -= 1
            elif u.op == Op.Plus:
                u.constValue = v1
            elif u.op == Op.Minus:
                u.constValue = -v1
            elif u.op == Op.Not:
                u.constValue = not v1
            else:
                self.addError("Unsupported unary operator: " + Op[u.op] +"in ConstFolder",u);

