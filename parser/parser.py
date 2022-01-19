import argparse

def parse_args():
  parser = argparse.ArgumentParser()
  parser.add_argument("source")

  return parser.parse_args()

def tokenize(source):
  token = ""
  chars = iter(source.read())

  while True:
    try:
      char = next(chars)
    except StopIteration:
      if token:
        yield token
      return

    if char in [" ", "\t", "\n", "\r"]:
      if token:
        if token[0] == "#":
          while char != "\n":
            char = next(chars)
        else:
          yield token
      token = ""
      continue
    token += char

def pairwize(generator):
    a = next(generator)
    b = next(generator)
  
    while True:
      yield a, b
      a = b
      try:
        b = next(generator)
      except StopIteration:
        yield a, None
        return

"""

program:
  | top_statement program
  | top_statement

top_statement:
  | statement
  | 'function' function_declaration

function_declaration:
  | identifier num_literal num_literal statement_list 'end'
  | identifier num_literal num_literal 'end'

statement_list:
  | statement
  | statement statement_list

statement:
  | 'while' statement_list 'do' statement_list 'end'
  | 'while' statement_list 'do' 'end'
  | 'if' if_body
  | num_literal
  | string_literal
  | builtin
  | identifier

if_body:
  | statement_list 'do' statement_list 'elif' if_body
  | statement_list 'do' statement_list 'end'

builtin:
  | '<'
  | '=='
  | '->'
  | '.'
  | 'print'
  | 'dup'
  | 'return'
""" 

class Tokenizer:
  def __init__(self, source_file):
    self.pairs = pairwize(tokenize(source_file))
    self.buf = None

  def expect(self, token):
    if self.peek() == token:
      return token
    return None

  def consume_maybe(self, token):
    if self.peek() == token:
      return self.consume()

  def consume_only(self, token):
    if self.peek() == token:
      return self.consume()
    else:
      raise SyntaxError(f"expected {token} but found {self.peek()}")

  def peek(self):
    if self.buf:
      return self.buf[0]
    try:
      self.buf = next(self.pairs) 
    except StopIteration:
      self.buf = (None, None)
    return self.buf[0]
  
  def consume(self):
    if self.buf:
      tok = self.buf[0]
      self.buf = None
      return tok

    tok = next(self.pairs)[0]
    return tok

class FunctionDeclaration:
    def __init__(self, name, num_args, num_returns):
      self.name = name
      self.num_args = num_args
      self.num_returns = num_returns
      self.body = None

    def dump(self, level=0):
      print("\t"*level, self.num_args, self.num_returns)
      self.body.dump(level+1)

class StatementList:
    def __init__(self):
      self.statements = []

    def append(self, statement):
      self.statements.append(statement)

    def dump(self, level=0):
      for statement in self.statements:
        statement.dump(level)

class Statement:
    def __init__(self, token):
      self.token = token

    def dump(self, level):
        print("\t" * level, self.token)

class WhileStatement:
    def __init__(self, condition):
      self.condition = condition
      self.body = None

    def dump(self, level):
      print("\t"* level, "While")
      self.condition.dump(level+1)

      if self.body:
        print("\t"*level, "Do")
        self.body.dump(level + 1)

      print("\t"* level, "End")

class IfStatement:
    def __init__(self, condition, body):
      self.condition = condition 
      self.body = body
      self.else_body = None

    def dump(self, level):
      print("\t" * level, "If")
      self.condition.dump(level + 1)

      if self.body:
        print("\t" * level, "Do")
        self.body.dump(level+1)

      if self.else_body:
        self.else_body.dump(level+1)

      print("\t" * level, "End")

def parse_program(tokens):
    """
    program:
      | top_statement program
      | top_statement
    """
    functions = {}
    top_statements = StatementList()
    while tokens.peek():
      if tokens.consume_maybe("function"):
        function_declaration = parse_function_declaration(tokens) 
        functions[function_declaration.name] = function_declaration
      else:
        statement = parse_statement(tokens) 
        top_statements.append(statement)
    return functions, top_statements

def parse_function_declaration(tokens):
    """
    function_declaration:
      | identifier num_literal num_literal statement_list 'end'
      | identifier num_literal num_literal 'end'
    """
    name = tokens.consume()
    num_input_args = int(tokens.consume())
    num_output_args = int(tokens.consume())
    function = FunctionDeclaration(name, num_input_args, num_output_args)
    if tokens.consume_maybe("end"):
      return function

    function.body = parse_statement_list(tokens)
    
    tokens.consume_only("end")

    return function 

def parse_statement_list(tokens):
    """
    statement_list:
      | statement
      | statement statement_list
    """ 
    statement_list = StatementList()
    while True:
      statement = parse_statement(tokens) 
      statement_list.append(statement)

      if tokens.peek() in ["end", "elif", "do"]:
        return statement_list

def parse_statement(tokens):
    """
    statement:
      | 'while' statement_list 'do' statement_list 'end'
      | 'while' statement_list 'do' 'end'
      | 'if' if_body
      | num_literal
      | string_literal
      | builtin
      | identifier
    """
    if tokens.consume_maybe("while"):
      condition = parse_statement_list(tokens)
      tokens.consume_only("do")
      statement = WhileStatement(condition)
      if tokens.consume_maybe("end"):
        return statement
       
      body = parse_statement_list(tokens)
      statement.body = body
      tokens.consume_only("end")
      return statement 

    if tokens.consume_maybe("if"):
      return parse_if_body(tokens)
    
    token = tokens.consume() 
    return Statement(token)

def parse_if_body(tokens):
    """
    if_body:
      | statement_list 'do' statement_list 'elif' if_body
      | statement_list 'do' statement_list 'end'
    """

    condition = parse_statement_list(tokens)
    tokens.consume_only("do")
    body = parse_statement_list(tokens)
    if_statement = IfStatement(condition, body)
    if tokens.consume_maybe("elif"):
      if_statement.else_body = parse_if_body(tokens)
      return if_statement
    tokens.consume_only("end")
    return if_statement



def main():
  args = parse_args()

  for f in [args.source]:
    with open(f) as source_file:
      tokenizer = Tokenizer(source_file)
      functions, statements = parse_program(tokenizer)
  

      for name, definition in functions.items():
        print("Function", name)
        definition.dump()
      
      print("Top level", name)
      statements.dump()
if __name__ == '__main__':
  main()
