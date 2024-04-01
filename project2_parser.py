# If you haven't read the P2 post in campuswire, read it before you continue.

# If you have working lexer of project 1, then you are good to go, you just
# need few modifications in the lexer. I believe you are better off, if you
# just extend it.

# If you don't have a working lexer for project 1, we have provide a skelton of 
# lexer. You need to complete the functions commented with tokenize.

# newline lexer.
class Token:
    def __init__(self, token_type, value=None):
        self.type = token_type
        self.value = value

class Lexer:
    def __init__(self, code):
        self.code = code
        self.position = 0
        self.currentCharachter = self.code[self.position] if self.code else None #Set current charachter to the current position and 
        #if the code is empty than return None

        #definie operater, keyword, separators, and letters
        self.operators = {"+", "-", "/", "*", "=", "==", ">", "<", ">=", "<="}
        self.keywords = {"if", "while", "else", "elif","then"}
        self.separators = {",", ";", "(", ")"}
        self.letters = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")#Letters is uppercase and lowercase letters


    def error(self):
        raise Exception('Invalid character')

    def advance(self):#helper function to advance the position by oe
        self.position += 1#move the position
        if self.position < len(self.code):#checks to make sure the position is within bound
            self.currentCharachter = self.code[self.position]#if it is update the current charachrter to the position
        else:
            self.currentCharachter = None #otherwise if it is outside set it to none


    def checkNext(self):#helper function to check the next character without moving foward to see what comes next 
        checkNext_pos = self.position + 1#gets the position of the next charachter
        if checkNext_pos < len(self.code):#makes sure the charachter is within bounds
            return self.code[checkNext_pos]#returns the charachters position if it is in bound
        else:
            return None#otherwise if its out of bounds return none
        
    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    # implement
    def number(self):
        stringofNumbers = '' #creates a emptry string to collect numbers 
        decimalPoints = 0
        while self.currentCharachter is not None and (self.currentCharachter.isdigit() or self.currentCharachter == '.'):#keep going until
            if self.currentCharachter.isdigit():
                stringofNumbers += self.currentCharachter
            elif self.currentCharachter == '.':
                decimalPoints += 1
                if decimalPoints == 1:
                    stringofNumbers += self.currentCharachter
            else: 
                break
        self.advance()

        if decimalPoints == 0:
            return (stringofNumbers, "NUMBER") #Returns the string of Numbers and tags it as "NUMBER"
        else:
            return (stringofNumbers, "FLOAT")#Returns the string of Numbers and tags it as "NUMBER"
    # implement
    def identifier(self):
        stringOfID = ''
        while self.currentCharachter is not None and (self.currentCharachter.isalnum() or self.currentCharachter == '_'):
            stringOfID += self.currentCharachter #if it does then it appends it to the string of identifieds and keywords
            self.advance()
        return stringOfID
            
        
        return (stringOfID, token_type)#returns the stringOFID and the token type
    def get_next_token(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            elif self.current_char.isdigit():
                res = self.number()
                if res[1] == 1:
                    return Token('FNUMBER', res[0])
                else:
                    return Token('NUMBER', res[0])
            elif self.current_char.isalpha() or self.current_char == '_':
                return self.keyword_or_identifier()
            elif self.current_char == '+' or self.current_char == '-' or self.current_char == '*' or self.current_char == '/':
                return self.operator()
            elif self.current_char == '(' or self.current_char == ')':
                token = Token('PARENTHESIS', self.current_char)
                self.advance()
                return token
            elif self.current_char == '{' or self.current_char == '}':
                token = Token('SCOPE', self.current_char)
                self.advance()
                return token
            elif self.current_char == '\n':  # Change delimiter to newline character
                token = Token('DELIMITER', self.current_char)
                self.advance()
                return token
            elif self.current_char == '!':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '!=')
                else:
                    self.error()

            elif self.current_char == '=':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '==')
                else:
                    return Token('OPERATOR', '=')

            elif self.current_char == '<':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '<=')
                else:
                    return Token('OPERATOR', '<')
            elif self.current_char == '>':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '>=')
                else:
                    return Token('OPERATOR', '>')
            else:
                self.error()
        return Token('EOF')

    #implement
    def keyword_or_identifier(self,stringOfID):
        token_type = "KEYWORD" if stringOfID in self.keywords else "IDENTIFIER" #we check if the stringOFID is in the keywords or identifier 
        return (stringOfID, token_type)#returns the stringOFID and the token type
    #implement
    def operator(self):
        op = self.currentCharachter#sets op(operetator) to the current charachter
        self.checkNext()
        if op + self.checkNext() in self.operators: #checks if the current and next character is a operator
            op += self.checkNext()#if it is then we append it to op
            self.advance()
        self.advance()
        return (op, "OPERATOR") #returns the operator and assigns t as "OPERATOR"

# Parse Tree Node definitions.
# Don't need to modify these definitions for the completion of project 2.

# But if you are interested in modifying these definitions for
# learning purposes. Then Don't whatever you want.

class Node:
    pass

class ProgramNode(Node):
    def __init__(self, statements):
        self.statements = statements

class DeclarationNode(Node):
    def __init__(self, identifier, expression, myType):
        self.identifier = identifier
        self.expression = expression
        self.type       = myType

class AssignmentNode(Node):
    def __init__(self, identifier, expression):
        self.identifier = identifier
        self.expression = expression

class IfStatementNode(Node):
    def __init__(self, condition, if_block, else_block):
        self.condition = condition
        self.if_block = if_block
        self.else_block = else_block

class WhileLoopNode(Node):
    def __init__(self, condition, loop_block):
        self.condition = condition
        self.loop_block = loop_block

class ConditionNode(Node):
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

class ArithmeticExpressionNode(Node):
    def __init__(self, operator, left, right, myType):
        self.operator = operator
        self.left = left
        self.right = right
        self.type  = myType

class TermNode(Node):
    def __init__(self, operator, left, right, myType):
        self.operator = operator
        self.left = left
        self.right = right
        self.type  = myType

class FactorNode(Node):
    def __init__(self, value, myType):
        self.value = value
        self.type = myType




# final parser - student copy

# Skelton of Parser class.
# For project 1, we should have implemented parser that returns a string representation.
# For project 2:
  # 1. You have to build the Parse tree with the node definitions given to you. The core
  # logic of how to parse the lanague will not differ, but you to have create Tree node
  # whereever you are creating tuple in the project 1.
  # 2. Implement symbol table and scoping rules. 
  #   Hint: You can use stack to model the nested scopes and a dictionary to store identifiers
  #   and its type.

  # For those who are interested, you call print_parse_tree to view the text representation
  # of Parse Tree.


class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()
        # implement symbol table and scopes
       
        self.messages = []

    def print_parse_tree(self, node, indent=0):
        message = ""
        if isinstance(node, ProgramNode):
            message += '  ' * indent + 'Program\n'
            for statement in node.statements:
                message += self.print_parse_tree(statement, indent + 1)
        elif isinstance(node, DeclarationNode):
            message += '  ' * indent + 'Declaration: ' + node.identifier + '\n'
            message += self.print_parse_tree(node.expression, indent + 1)
        elif isinstance(node, AssignmentNode):
            message += '  ' * indent + 'Assignment: ' + node.identifier + '\n'
            message += self.print_parse_tree(node.expression, indent + 1)
        elif isinstance(node, IfStatementNode):
            message += '  ' * indent + 'If Statement\n'
            message += self.print_parse_tree(node.condition, indent + 1)
            message += '  ' * indent + 'Then Block:\n'
            for statement in node.if_block:
                message += self.print_parse_tree(statement, indent + 2)
            if node.else_block:
                message += '  ' * indent + 'Else Block:\n'
                for statement in node.else_block:
                    message += self.print_parse_tree(statement, indent + 2)
        elif isinstance(node, WhileLoopNode):
            message += '  ' * indent + 'While Loop\n'
            message += self.print_parse_tree(node.condition, indent + 1)
            message += '  ' * indent + 'Loop Block:\n'
            for statement in node.loop_block:
                message += self.print_parse_tree(statement, indent + 2)
        elif isinstance(node, ConditionNode):
            message += '  ' * indent + 'Condition : with operator ' + node.operator + '\n'
            message += '  ' * indent + 'LHS\n'
            message += self.print_parse_tree(node.left, indent + 2)
            message += '  ' * indent + 'RHS\n'
            message += self.print_parse_tree(node.right, indent + 2)
        elif isinstance(node, ArithmeticExpressionNode):
            message += '  ' * indent + 'Arithmetic Expression: ' + node.operator + '\n'
            message += self.print_parse_tree(node.left, indent + 1)
            message += self.print_parse_tree(node.right, indent + 1)
        elif isinstance(node, TermNode):
            message += '  ' * indent + 'Term: ' + node.operator + '\n'
            message += self.print_parse_tree(node.left, indent + 1)
            message += self.print_parse_tree(node.right, indent + 1)
        elif isinstance(node, FactorNode):
            message += '  ' * indent + 'Factor: ' + str(node.value) + '\n'

        return message


    def error(self, message):
        self.messages.append(message)

    def eat(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error(f'Expected token of type {token_type}, but found {self.current_token.type}')

    # enter the new scope in the program
    def enter_scope(self, scope_prefix):
        
    # leave the current scope
    def leave_scope(self):
        
    # return the current scope
    def current_scope(self):
        

    def checkVarDeclared(self, identifier):
        
        if #implement :
            self.error(f'Variable {identifier} has already been declared in the current scope')

    def checkVarUse(self, identifier):
        # check var declared, so we can use it.
        self.error(f'Variable {identifier} has not been declared in the current or any enclosing scopes')


    # return false when types mismatch, otherwise ret true
    def checkTypeMatch(self, vType, eType, var, exp):

    # return its type or None if not found
    def getMyType(self, identifier):
      

    def parse_program(self):
        statements = []
        while self.current_token.type != 'EOF':
            statements.append(self.parse_statement())
            if self.current_token.type == 'DELIMITER':
                self.eat('DELIMITER')
        return ProgramNode(statements)


    def parse_statement(self):


    def parse_declaration(self):


    def parse_assignment(self):


    def parse_if_statement(self):
 

    def parse_while_loop(self):
        
        return WhileLoopNode(condition, loop_block)
    
    # No need to check type mismatch here.
    def parse_condition(self):
        
        return ConditionNode(left, operator, right)

    def parse_arithmetic_expression(self):

        

    def parse_term(self):


    def parse_factor(self):
