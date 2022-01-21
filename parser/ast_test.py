import ast

def foo():
    i = 0
    while True:
        if i == 10:
            break
        i += 1

print(ast.dump(ast.parse("break")))