'''
类型体系
'''
from typing import List

class Type:
    def __init__(self, name:str):
        self.name = name

    def LE(self, type2) -> bool:
        '''
        当前类型是否小于等于type2
        type2:Type
        '''
        pass

    def accept(self, visitor) -> any:
        '''
        visitor模式, 用于生成字节码等
        visitor:TypeVisitor
        '''
        pass

    def hasVoid(self) -> bool:
        '''
        类型中是否包含void
        '''
        pass

    def toString(self) -> str:
        pass

    @staticmethod
    def getUpperBound(type1, type2):
        '''
        type1:Type
        type2:Type
        '''
        if type1 == SysTypes.Any or type2 == SysTypes.Any:
            return SysTypes.Any
        else:
            if type1.LE(type2):
                return type2
            elif type2.LE(type1):
                return type1
            else:
                return UnionType([type1, type2])

    @staticmethod
    def isSimpleType(t):
        '''
        t:Type
        '''
        return t.upperTypes is not None

    @staticmethod
    def isUnionType(t):
        '''
        t:Type
        '''
        return t.types is not None

    @staticmethod
    def isFunctionType(t):
        '''
        t:Type
        '''
        return t.returnType is not None

'''
简单的类型，可以有一到多个父类型
'''
class SimpleType(Type):
    def __init__(self, name:str, upperTypes:List=[]):
        '''
        upperTypes:List[SimpleType]
        '''
        super().__init__(name)
        self.upperTypes = upperTypes

    def hasVoid(self) -> bool:
        if self is SysTypes.Void:
            return true
        else:
            for t in self.upperTypes:
                if t.hasVoid():
                    return True
            return False

    def toString(self) -> str:
        upperTypeNames:str = "["
        for ut in self.upperTypes:
            upperTypeNames += ut.name + ", "
        upperTypeNames += "]"
        return "SimpleType {name: " + self.name + ", upperTypes: " + upperTypeNames + "}"

    def LE(self, type2:Type) -> bool:
        '''
        当前类型是否小于等于type2
        '''
        if type2 == SysTypes.Any:
            return True
        elif self == SysTypes.Any:
            return False
        elif self == type2:
            return True
        elif Type.isSimpleType(type2):
            t = type2
            if self.upperTypes.indexOf(t) != -1:
                return True
            else:
                ##看父类型中有没有type2的子类型
                for upperType in self.upperTypes:
                    if upperType.LE(type2):
                        return True
                return False
        elif Type.isUnionType(type2):
            t = type2
            if t.types.indexOf(self) != -1:
                return True
            else:
                ##是联合类型中某一个类型的子类型
                for t2 in t.types:
                    if self.LE(t2):
                        return True
                return False
        else:
            return False

    def accept(self, visitor) ->any:
        '''
        visitor模式
        visitor:TypeVisitor
        '''
        return visitor.visitSimpleType(self)

class SysTypes:
    ##所有类型的父类型
    Any = SimpleType("any", [])

    String = None
    Number = None
    Boolean = None
    Integer = None
    Decimal = None
    def __init__(self):
        '''
        不知道怎么解决类静态变量报错问题 String = SysTypes.Any 会报错'SysTypes' is not defined，
        先在方法内部初始化 String, Number等变量
        '''
        SysTypes.String = SimpleType("string", [SysTypes.Any])
        SysTypes.Number = SimpleType("number", [SysTypes.Any])
        SysTypes.Boolean = SimpleType("boolean", [SysTypes.Any])
        SysTypes.Integer = SimpleType("integer", [SysTypes.Number])
        SysTypes.Decimal = SimpleType("decimal", [SysTypes.Number])

    ##基础类型
    # String = SimpleType("string", [SysTypes.Any])
    # Number = SimpleType("number", [SysTypes.Any])
    # Boolean = SimpleType("boolean", [SysTypes.Any])

    ##所有类型的子类型
    Null = SimpleType("null")
    Undefined = SimpleType("undefined")

    ##函数没有任何返回值的情况
    ##如果作为变量的类型，则智能赋值为null和undefined
    Void = SimpleType("void")

    ##两个Number子类型
    # Integer = SimpleType("integer", [SysTypes.Number])
    # Decimal = SimpleType("decimal", [SysTypes.Number])

    @staticmethod
    def isSysType(t:Type):
        return t == SysTypes.Any or\
            t == SysTypes.String or\
            t == SysTypes.Number or\
            t == SysTypes.Boolean or\
            t == SysTypes.Null or\
            t == SysTypes.Undefined or\
            t == SysTypes.Void or\
            t == SysTypes.Integer or\
            t == SysTypes.Decimal
#初始化静态变量
SysTypes()

class FunctionType(Type):
    index:int = 0 #序号，用于给函数类型命名
    def __init__(self, returnType:Type = SysTypes.Void, paramTypes:List[Type] = [], name:str or None = None):
        super().__init__("@function")
        self.returnType = returnType
        self.paramTypes = paramTypes
        if type(name) is str:
            self.name = name
        else:
            self.name = "@function"+str(FunctionType.index)
            FunctionType.index+=1

    def hasVoid(self) -> bool:
        return self.returnType.hasVoid()

    def toString(self) -> str:
        paramTypeNames:str = "["
        for ut in self.paramTypes:
            paramTypeNames += ut.name + ", "
        paramTypeNames += "]"
        return "FunctionType {name: " + self.name + ", returnType: "+self.returnType.name + ", paramTypes: "+paramTypeNames+"}"

    def LE(self, type2:Type) -> bool:
        if type2 == SysTypes.Any:
            return True
        elif self == type2:
            return True
        elif Type.isUnionType(type2):
            t = type2
            if t.types.indexOf(self) != -1:
                return True
            else:
                return False
        else:
            return False

    def accept(self, visitor) -> any:
        '''
        visitor:TypeVisitor
        '''
        return visitor.visitFunctionType(self)

class UnionType(Type):
    index:int = 0
    def __init__(self, types:List[Type], name:str or None=None):
        super().__init__("@union")
        self.types = types

        if type(name) is str:
            self.name = name
        else:
            self.name = "@union"+(UnionType.index)
            UnionType.index+=1

    def hasVoid(self) -> bool:
        for t in self.types:
            if t.hasVoid():
                return True
        return False

    def toString(self) -> str:
        typeNames:str = "["
        for ut in self.types:
            typeNames += ut.name + ", "
        typeNames += "]"
        return "UnionType {name: " + self.name + ", types: " + typeNames + "}"

    def LE(self, type2:Type) -> bool:
        if type2 == SysTypes.Any:
            return True
        elif Type.isUnionType(type2):
            for t1 in self.types:
                found = False
                for t2 in type2.types:
                    if t1.LE(t2):
                        found = True
                        break
                if not found:
                    return False
            return True
        else:
            return False

    def accept(self, visitor) -> any:
        '''
        visitor:TypeVisitor
        '''
        visitor.visitUnionType(self)

class TypeVisitor:
    def visit(self, t:Type) -> any:
        '''
        t:Type
        '''
        return t.accept(self)

    def visitSimpleType(self, t:SimpleType) -> any:
        pass

    def visitFunctionType(self, t:FunctionType) -> any:
        pass

    def visitUnionType(self, t:UnionType) -> any:
        pass




