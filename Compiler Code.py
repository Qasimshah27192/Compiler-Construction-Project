import tkinter as tk
from tkinter import filedialog, messagebox, scrolledtext
import ply.lex as lex
import ply.yacc as yacc

# Token definitions
tokens = (
    'NUMBER', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'LPAREN', 'RPAREN', 'IDENTIFIER', 'ASSIGN', 'PRINT',
    'KEYWORD', 'DATA_TYPE', 'CONSTANT', 'LITERAL',
    'LCURLY', 'RCURLY', 'SEPARATOR', 'STATEMENT_END', 'GREATER', 'LESS', 'IF', 'ELSE', 'WHILE'
)

# Regular expressions for simple tokens
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_ASSIGN = r'='
t_LCURLY = r'\{'
t_RCURLY = r'\}'
t_SEPARATOR = r','
t_STATEMENT_END = r'\.'
t_GREATER = r'>'
t_LESS = r'<'

# Reserved words
reserved = {
    'print': 'PRINT',
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'Num': 'DATA_TYPE',
    'Fl': 'DATA_TYPE',
    'Str': 'DATA_TYPE',
    'Bool': 'DATA_TYPE',
    'Char': 'DATA_TYPE',
    'true': 'LITERAL',
    'false': 'LITERAL'
}

# Regular expressions with actions
def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'IDENTIFIER')    # Check for reserved words
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_CONSTANT(t):
    r'\".*?\"|\'.*?\''
    return t

def t_LITERAL(t):
    r'\b(?:true|false)\b'
    return t

# Ignored characters
t_ignore = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_COMMENT(t):
    r'\#.*'
    pass

def t_error(t):
    error_message = f"Lexical error: Undefined character '{t.value[0]}' at line {t.lexer.lineno}"
    print(error_message)
    errors.append(error_message)
    t.lexer.skip(1)

lexer = lex.lex()

# Symbol table
symbol_table = {}

# List to store errors
errors = []

# Define grammar rules
def p_program(p):                   
    'program : statement_list'
    p[0] = ('program', p[1])                    #store list statement result in P[1] and then p[1] store in p[0]

def p_statement_list(p):
    '''statement_list : statement
                      | statement statement_list'''
    if len(p) == 2:                         #if p length==2 then p[0] stores p[1]
        p[0] = [p[1]]
    else:                                   #if p length is greater than 2 then concatenate p1 and p2
        p[0] = [p[1]] + p[2]

def p_statement(p):
    '''statement : declaration
                 | assignment
                 | print_statement
                 | if_statement
                 | while_statement'''
    p[0] = p[1]

def p_declaration(p):
    '''declaration : DATA_TYPE IDENTIFIER STATEMENT_END
                   | DATA_TYPE IDENTIFIER ASSIGN expression STATEMENT_END'''
    if len(p) == 4:
        if p[2] in symbol_table:
            error_message = f"Semantic error: Variable '{p[2]}' already declared"
            print(error_message)
            errors.append(error_message)
        else:
            symbol_table[p[2]] = {'type': p[1], 'value': None}
        p[0] = ('declaration', p[1], p[2])
    else:
        if p[2] in symbol_table:
            error_message = f"Semantic error: Variable '{p[2]}' already declared"
            print(error_message)
            errors.append(error_message)
        else:
            symbol_table[p[2]] = {'type': p[1], 'value': p[4]}
        p[0] = ('declaration', p[1], p[2], p[4])

def p_assignment(p):
    'assignment : IDENTIFIER ASSIGN expression STATEMENT_END'
    if p[1] not in symbol_table:
        error_message = f"Semantic error: Undefined variable '{p[1]}'"
        print(error_message)
        errors.append(error_message)
    else:
        # Type checking for assignment
        var_type = symbol_table[p[1]]['type']
        expr_type = get_expression_type(p[3])
        if var_type != expr_type:
            error_message = f"Semantic error: Type mismatch in assignment to '{p[1]}'"
            print(error_message)
            errors.append(error_message)
        else:
            symbol_table[p[1]]['value'] = p[3]
    p[0] = ('assign', p[1], p[3])

def p_print_statement(p):
    'print_statement : PRINT LPAREN expression RPAREN STATEMENT_END'
    p[0] = ('print', p[3])

def p_if_statement(p):
    '''if_statement : IF expression LCURLY statement_list RCURLY
                    | IF expression LCURLY statement_list RCURLY ELSE LCURLY statement_list RCURLY'''
    if len(p) == 6:
        p[0] = ('if', p[2], p[4])
    else:
        p[0] = ('if_else', p[2], p[4], p[8])

def p_while_statement(p):
    'while_statement : WHILE expression LCURLY statement_list RCURLY'
    p[0] = ('while', p[2], p[4])

def p_expression(p):
    '''expression : term
                  | term PLUS expression
                  | term MINUS expression
                  | term GREATER term
                  | term LESS term
                  | CONSTANT'''
    if len(p) == 2:
        p[0] = p[1]
    elif p[2] in ('+', '-', '>', '<'):
        p[0] = (p[2], p[1], p[3])
    elif p[1] == 'CONSTANT':
        p[0] = ('string', p[1])

def p_term(p):
    '''term : factor
            | factor TIMES term
            | factor DIVIDE term'''
    if len(p) == 2:
        p[0] = p[1]
    elif p[2] == '*':
        p[0] = ('*', p[1], p[3])
    elif p[2] == '/':
        p[0] = ('/', p[1], p[3])

def p_factor(p):
    '''factor : NUMBER
              | IDENTIFIER
              | LPAREN expression RPAREN'''
    if isinstance(p[1], int):
        p[0] = ('number', p[1])
    elif isinstance(p[1], str):
        if p[1] not in symbol_table:
            error_message = f"Semantic error: Undefined identifier '{p[1]}'"
            print(error_message)
            errors.append(error_message)
        p[0] = ('identifier', p[1])
    else:
        p[0] = p[2]

def p_error(p):
    if p:
        error_message = f"Syntax error at '{p.value}'"
        print(error_message)
        errors.append(error_message)
    else:
        error_message = "Syntax error at EOF"
        print(error_message)
        errors.append(error_message)

parser = yacc.yacc()

# Utility function to get the type of an expression
def get_expression_type(expr):
    if isinstance(expr, tuple):
        if expr[0] == 'number':
            return 'Num'
        elif expr[0] == 'identifier':
            return symbol_table.get(expr[1], {}).get('type')
    return None

# Code generation functions
assembly_code = []

def generate_code_for_node(node):
    if not isinstance(node, tuple):
        return
    
    node_type = node[0]
    if node_type == 'program':
        for stmt in node[1]:
            generate_code_for_node(stmt)
    elif node_type == 'declaration':
        # Declarations don't need code, handled in assignments
        pass
    elif node_type == 'assign':
        var_name = node[1]
        expr = node[2]
        code = generate_code_for_expression(expr)
        assembly_code.append(f"STORE {var_name}")
    elif node_type == 'print':
        expr = node[1]
        code = generate_code_for_expression(expr)
        assembly_code.append("PRINT")
    elif node_type == 'if':
        expr = node[1]
        stmt_list = node[2]
        code = generate_code_for_expression(expr)
        label = new_label()
        assembly_code.append(f"JZ {label}")
        for stmt in stmt_list:
            generate_code_for_node(stmt)
        assembly_code.append(f"{label}:")
    elif node_type == 'if_else':
        expr = node[1]
        stmt_list_if = node[2]
        stmt_list_else = node[3]
        code = generate_code_for_expression(expr)
        label_else = new_label()
        label_end = new_label()
        assembly_code.append(f"JZ {label_else}")
        for stmt in stmt_list_if:
            generate_code_for_node(stmt)
        assembly_code.append(f"JMP {label_end}")
        assembly_code.append(f"{label_else}:")
        for stmt in stmt_list_else:
            generate_code_for_node(stmt)
        assembly_code.append(f"{label_end}:")
    elif node_type == 'while':
        expr = node[1]
        stmt_list = node[2]
        label_start = new_label()
        label_end = new_label()
        assembly_code.append(f"{label_start}:")
        code = generate_code_for_expression(expr)
        assembly_code.append(f"JZ {label_end}")
        for stmt in stmt_list:
            generate_code_for_node(stmt)
        assembly_code.append(f"JMP {label_start}")
        assembly_code.append(f"{label_end}:")
    elif node_type in ('+', '-', '*', '/'):
        left_expr = node[1]
        right_expr = node[2]
        left_code = generate_code_for_expression(left_expr)
        right_code = generate_code_for_expression(right_expr)
        if node_type == '+':
            assembly_code.append("ADD")
        elif node_type == '-':
            assembly_code.append("SUB")
        elif node_type == '*':
            assembly_code.append("MUL")
        elif node_type == '/':
            assembly_code.append("DIV")

def generate_code_for_expression(expr):
    if expr[0] == 'number':
        assembly_code.append(f"PUSH {expr[1]}")
    elif expr[0] == 'identifier':
        assembly_code.append(f"PUSH {expr[1]}")
    elif expr[0] in ('+', '-', '*', '/'):
        generate_code_for_node(expr)

# Label generation
label_counter = 0
def new_label():
    global label_counter
    label_counter += 1
    return f"L{label_counter}"

# Virtual Machine to execute the generated assembly code
def execute_assembly_code():
    stack = []
    variables = {}
    pc = 0  # Program counter
    output = []

    while pc < len(assembly_code):
        instruction = assembly_code[pc]
        parts = instruction.split()
        opcode = parts[0]

        if opcode == "PUSH":
            value = parts[1]
            if value.isdigit():
                stack.append(int(value))
            else:
                stack.append(variables.get(value, 0))
        elif opcode == "STORE":
            var_name = parts[1]
            variables[var_name] = stack.pop()
        elif opcode == "ADD":
            b = stack.pop()
            a = stack.pop()
            stack.append(a + b)
        elif opcode == "SUB":
            b = stack.pop()
            a = stack.pop()
            stack.append(a - b)
        elif opcode == "MUL":
            b = stack.pop()
            a = stack.pop()
            stack.append(a * b)
        elif opcode == "DIV":
            b = stack.pop()
            a = stack.pop()
            stack.append(a // b)
        elif opcode == "PRINT":
            output.append(stack.pop())
        elif opcode == "JZ":
            label = parts[1]
            if stack.pop() == 0:
                pc = assembly_code.index(f"{label}:") - 1
        elif opcode == "JMP":
            label = parts[1]
            pc = assembly_code.index(f"{label}:") - 1

        pc += 1
    
    return output

# GUI setup
root = tk.Tk()
root.title("Compiler GUI")

# Code editor
code_editor = scrolledtext.ScrolledText(root, width=60, height=20, wrap=tk.WORD)
code_editor.pack(pady=10)

# Buttons
button_frame = tk.Frame(root)
button_frame.pack(pady=5)

def open_file():
    file_path = filedialog.askopenfilename()
    if file_path:
        with open(file_path, 'r') as file:
            code_editor.delete(1.0, tk.END)
            code_editor.insert(1.0, file.read())

open_button = tk.Button(button_frame, text="Open File", command=open_file)
open_button.grid(row=0, column=0, padx=5)

def lex_analyze():
    code = code_editor.get(1.0, tk.END)
    lexer.input(code)
    output = ""
    for token in lexer:
        output += f"Token: {token.type}, Value: {token.value}\n"
    messagebox.showinfo("Lexical Analysis Output", output)

lex_button = tk.Button(button_frame, text="Lexical Analyze", command=lex_analyze)
lex_button.grid(row=0, column=1, padx=5)

def syntax_analyze():
    code = code_editor.get(1.0, tk.END)
    try:
        symbol_table.clear()
        errors.clear()
        parser.parse(code)
        if errors:
            error_message = "Errors found during syntax analysis:\n" + "\n".join(errors)
        else:
            error_message = "No syntax errors found."
        messagebox.showinfo("Syntax Analysis Output", f"{error_message}\n\nSymbol Table: {symbol_table}")
    except Exception as e:
        errors.append(str(e))
        messagebox.showerror("Syntax Analysis Error", "\n".join(errors))

syntax_button = tk.Button(button_frame, text="Syntax Analyze", command=syntax_analyze)
syntax_button.grid(row=0, column=2, padx=5)

def semantic_analyze():
    code = code_editor.get(1.0, tk.END)
    try:
        symbol_table.clear()
        errors.clear()
        parser.parse(code)
        if errors:
            error_message = "Errors found during semantic analysis:\n" + "\n".join(errors)
        else:
            error_message = "No semantic errors found."
        messagebox.showinfo("Semantic Analysis Output", error_message)
    except Exception as e:
        errors.append(str(e))
        messagebox.showerror("Semantic Analysis Error", "\n".join(errors))

semantic_button = tk.Button(button_frame, text="Semantic Analyze", command=semantic_analyze)
semantic_button.grid(row=0, column=3, padx=5)

def generate_code():
    code = code_editor.get(1.0, tk.END)
    try:
        symbol_table.clear()
        errors.clear()
        result = parser.parse(code)
        if errors:
            error_message = "Errors found during parsing:\n" + "\n".join(errors)
            messagebox.showerror("Code Generation Error", error_message)
        else:
            assembly_code.clear()
            generate_code_for_node(result)
            output = "\n".join(assembly_code)
            messagebox.showinfo("Generated Assembly Code", output)
    except Exception as e:
        errors.append(str(e))
        messagebox.showerror("Code Generation Error", "\n".join(errors))

code_button = tk.Button(button_frame, text="Generate Code", command=generate_code)
code_button.grid(row=0, column=4, padx=5)

def assembly_code_to_output():
    if not assembly_code:
        messagebox.showinfo("Assembly Code Output", "No assembly code generated yet.")
    else:
        output = execute_assembly_code()
        messagebox.showinfo("Assembly Code Output", f"Output: {output}")

assembly_button = tk.Button(button_frame, text="Assembly code to output", command=assembly_code_to_output)
assembly_button.grid(row=0, column=5, padx=5)

root.mainloop()
