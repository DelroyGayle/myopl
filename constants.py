import string

#######################################
# CONSTANTS
#######################################

DIGITS = "0123456789"
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS
NUMBER_CHARS = DIGITS + ".eE"
ERR = {}

#######################################
# ERRORS
#######################################

ERRORS = {
  # SYNTAX ERRORS
  "tokens_out_of_place": "Token cannot appear after previous tokens",
  "string_expected": "Expected string",
  "statement_syntax_error": ("Expected 'RETURN', 'CONTINUE', 'BREAK', 'VAR', "
                             "'IF', FOR', 'WHILE', 'FUN', "
                             "int, float, identifier, "
                             "'+', '-', '(', '[' or 'NOT'"),
  "identifier_expected":  "Expected identifier",
  "equal_expected": "Expected '='",
  "expr_syntax_error": ("Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', "
                        "int, float, identifier, '+', '-', '(', '[' or 'NOT'"),
  "comp_syntax_error": ("Expected int, float, identifier, '+', '-', '(', '[', "
                        "'IF', 'FOR', 'WHILE', 'FUN' or 'NOT'"),
  "arg1_syntax_error": ("Expected ')', 'VAR', 'IF', 'FOR', 'WHILE', "
                        "'FUN', int, float, identifier, "
                        "'+', '-', '(', '[' or 'NOT'"),
  "comma_rparen_expected": "Expected ',' or ')'",
  "lparen_expected": "Expected '('",
  "rparen_expected": "Expected ')'",
  "atom_syntax_error": ("Expected int, float, identifier, '+', '-', '(', '[', "
                        "IF', 'FOR', 'WHILE', 'FUN'"),
  "lbracket_expected": "Expected '['",
  "list_element_expected": ("Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', "
                            "int, float, identifier, "
                            "'+', '-', '(', '[', ']', or 'NOT'"),
  "comma_rbracket_expected": "Expected ',' or ']'",
  "end_expected": "Expected 'END'",
  "then_expected": "Expected 'THEN'",
  "for_expected": "Expected 'FOR'",
  "to_expected": "Expected 'TO'",
  "while_expected": "Expected 'WHILE'",
  "fun_expected": "Expected 'FUN'",
  "identifier_lparen_expected": "Expected identifier or '('",
  "identifier_rparen_expected": "Expected identifier or ')'",
  "arrow_NL_expected": "Expected '->' or NEWLINE",
  "arg_list_expected": "Argument must be a list",
  "arg1_list_expected": "First argument must be a list",
  "arg2_list_expected": "Second argument must be a list",
  "arg2_number_expected": "Second argument must be a string",
  # RUNTIME ERRORS
  "division_by_zero": "Division by zero",
  "modulus_by_zero": "Modulus using zero",
  "list_index_error": ("Element at this index could not be removed from "
                             "list because index is out of bounds"),
  "fetch_index_error": ("Element at this index could not be retrieved "
                        "from list because index is out of bounds"),
}
