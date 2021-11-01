'''
 * 编译过程中的错误信息
 * @version 0.1
 * @author czy
 * @license MIT
 * 
 * 当前特性：
 * 1.树状的符号表
 * 2.简单的引用消解：没有考虑声明的先后顺序，也没有考虑闭包
 * 3.简单的作用域
'''

from ast import AstNode
from scanner import Position

class CompilerError:
	def __init__(self, msg:str, beginPos:Position, isWarning = False):
		self.msg = msg
		self.beginPos = beginPos
		self.isWarning = isWarning


