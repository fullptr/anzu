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
  | function_declaration

function_declaration:
  | 'function' identifier num_literal num_literal statement_list 'end'
  | 'function' identifier num_literal num_literal 'end'

statement_list:
  | statement
  | statement statement_list

if_body:
  | statement_list 'do' statement_list 'elif' if_body
  | statement_list 'do' statement_list 'end'

statement:
  | 'while' statement_list 'do' statement_list 'end'
  | 'while' statement_list 'do' 'end'
  | 'if' if_body
  | num_literal
  | string_literal
  | 'return'
  | builtin
  | identifier

builtin:
  | '<'
  | '=='
  | '->'
  | '.'
  | 'print'
  | 'dup'
""" 

def main():
  args = parse_args()

  for f in [args.source]:
    with open(f) as source_file:
      token_stream = tokenize(source_file)
      pairs = pairwize(token_stream) 
      for token, next_token in pairs:
        print(token)

if __name__ == '__main__':
  main()
