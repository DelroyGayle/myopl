# MYOPL - Make Your Own Programming Language

## Introduction

This is my fork of David Callahan's code for my **"Make your own programming language in Python" tutorial series on [YouTube](https://www.youtube.com/playlist?list=PLZQftyCk7_SdoVexSmwy_tBgs7P0b97yD)**

This code is an interpreter for a BASIC-like language written in Python 3.

Will like to make some improvements to the code. The source base of this code is [Episode 14](https://github.com/davidcallanan/py-myopl-code/tree/master/ep14) of David Callahan's code

Would like to acknowledge [angelcaru](https://github.com/angelcaru/myopl-plus-plus) who has made many improvements<br>

I made the following additions:

- Modulus operator **%**
- Multi-line comments. `#* multi-line comment *#` (As shown by [angelcaru](https://github.com/angelcaru/myopl-plus-plus))
- Import statements. `IMPORT "./path/to/library/some_library.myopl"` (As shown by [angelcaru](https://github.com/angelcaru/myopl-plus-plus))
- Closures: In the original, `FUN wrapper(x) -> FUN haxxor() -> x; FUN defineX(x, f) -> f(); PRINT(defineX("hacked", wrapper("closure working")))` would print `hacked` instead of the expected result of `closure working`.<br>This functionality is now implemented properly. (As shown by [angelcaru](https://github.com/angelcaru/myopl-plus-plus))
- Escaped characters - The Lexer can handle
-   - \\"	Double Quotes	
-   - \\\\	Backslash	
-   - \n	New Line	
-   - \r	Carriage Return	
-   - \t	Tab	
-   - \b	Backspace	
-   - \f	Form Feed	
-   - \xhh	Hex value
-   Example: `"Hello,\x0AWorld"`
- Exponential Notation: Can add numbers such as `1e-3 1E2 +9e307 -1e-6`
- Added the commands BREAK and CONTINUE for loops
- RETURN can only be used in functions

## Various Tests

```
basic > 1/0
Traceback (most recent call last):
  File <stdin>, line 1, in <program>
Runtime Error: Division by zero

1/0
  ^


basic > 5%0
Traceback (most recent call last):
  File <stdin>, line 1, in <program>
Runtime Error: Modulus using zero

5%0
  ^

```
### RUN replaced with IMPORT

```
RUN("example.myopl")
WARNING: run() is deprecated. Use 'IMPORT' instead
Greetings universe!
loop, spoop
loop, spoop
loop, spoop
loop, spoop
loop, spoop
0
```

*REPLACED WITH*

```
IMPORT "example.myopl"  # NOTE: NO PARENTHESES
Greetings universe!
loop, spoop
loop, spoop
loop, spoop
loop, spoop
loop, spoop
0
```

### Test Closures

Using this file: TEST.MYOPL which contains
```
FUN wrapper(x) -> FUN haxxor() -> x; FUN defineX(x, f) -> f(); PRINT(defineX("hacked", wrapper("closure working")))
PRINT(1+3)

basic > IMPORT "test.MYOPL"
closure working
4
0
```

### Exponential Notation

```
basic > 1e-003
0.001

basic > 1e003 
1000.0

basic > 1E4 + 1E2
10100.0

basic > -1e-6
-1e-06

basic > +9e307
9e+307

basic > 10e306
1e+307

basic > -10e-306 
-1e-305

basic > -9.99999e307 
-9.99999e+307

basic > -9.99999e-307
-9.99999e-307

basic > 1e003
1000.0
basic > 1E4 * 1E2 
1000000.0
basic > 1E4 % 1E2 
0.0
```

**Syntax checks / Error handling**

```
basic > 1E.
Invalid Syntax: Floating point number in exponential notation expected

File <stdin>, line 1

1E.
^^

basic > 1E- 
Invalid Syntax: Floating point number in exponential notation expected

File <stdin>, line 1

1E-
^^^
```

**Added the limit of 1E308 for the magnitude of numbers**<br>
That is, any number >= 1E308  or <= -1E308 is considered an error

```
basic > 1E308
Invalid Syntax: Exponential number too large, its exponent is >= 308

File <stdin>, line 1

1E308
^^^^^

basic > 1E-308 
Invalid Syntax: Exponential number too small, its exponent is <= -308

File <stdin>, line 1

1E-308
^^^^^^

basic > 1E-309
Invalid Syntax: Exponential number too small, its exponent is <= -308

File <stdin>, line 1

1E-309
^^^^^^

basic > 1E7999 
Invalid Syntax: Exponential number too large, its exponent is >= 308

File <stdin>, line 1

1E7999
^^^^^^

basic > 10e307
Invalid Syntax: Exponential number too large, its exponent is >= 308

File <stdin>, line 1

10e307
^^^^^^
```

### Strings and Escaped Characters

```
basic > "\\abc\\"
"\abc\"

basic > "\\abc\\\n\t"

"\abc\
        "

basic > "\\\n\t"
"\
        "

basic > "\n\t\f\b"
"

"

basic > "\\\\\f\b\""  
"\\
""

basic > "\\\"\'\H"
"\"'H"

basic > "\r\n\"\b"
"
"
```

**Syntax checks / Error handling**

```
basic > "\\abc\\\n\t
Invalid Syntax: Unterminated String

File <stdin>, line 1

"\\abc\\\n\t
^^^^^^^^^^^^

basic > "\\\"
Invalid Syntax: Unterminated String

File <stdin>, line 1

"\\\"
^^^^^

```
**Test hex characters - must be of the format \xhh**

```
basic > "\x41"
"A"

basic > "\x48\x65\x6c\x6c\x6f"
"Hello"
basic > "\x48\x65\x6C\x6C\x6f" 
"Hello"

basic > "\<\x48\x65\x6c\x6c\x6f\>" 
"\<Hello\>"
```

**Syntax checks / Error handling**

```
basic > "\x48\x65\x6C\x6C\x6"
Invalid Syntax: Two Hex characters expected \xhh, h either 0-9 A-F a-f

File <stdin>, line 1

"\x48\x65\x6C\x6C\x6"
^^^^^^^^^^^^^^^^^^^^^

basic > "\x48\x65\x6C\x6C\x"  
Invalid Syntax: Two Hex characters expected \xhh, h either 0-9 A-F a-f

File <stdin>, line 1

"\x48\x65\x6C\x6C\x"
^^^^^^^^^^^^^^^^^^^^

basic > "\x4"                
Invalid Syntax: Two Hex characters expected \xhh, h either 0-9 A-F a-f

File <stdin>, line 1

"\x4"
^^^^^

basic > "\x4G" 
Invalid Syntax: Two Hex characters expected \xhh, h either 0-9 A-F a-f

File <stdin>, line 1

"\x4G"
^^^^^

basic > "\x4x" 
Invalid Syntax: Two Hex characters expected \xhh, h either 0-9 A-F a-f

File <stdin>, line 1

"\x4x"
^^^^^

basic > "\xx"  
Invalid Syntax: Two Hex characters expected \xhh, h either 0-9 A-F a-f

File <stdin>, line 1

"\xx"
^^^^

basic > "\xZ" 
Invalid Syntax: Two Hex characters expected \xhh, h either 0-9 A-F a-f

File <stdin>, line 1

"\xZ"
^^^^

basic > "\x"  
Invalid Syntax: Two Hex characters expected \xhh, h either 0-9 A-F a-f

File <stdin>, line 1

"\x"
^^^^
```

### IF/ELSE

```
basic > IF 4 THEN 5
5
basic > IF 0 THEN 5 ELSE 100
100

basic > IF 4 THEN; 5; PRINT("YES") END 
YES
0

basic > IF 0 THEN; 5; PRINT("YES") ELSE; PRINT("NO") END  
NO
0
```

### BREAK, CONTINUE & RETURN<br>

*TEST FOR/BREAK/CONTINUE*

```
basic > VAR a = []
[]
basic > FOR i=0 TO 10 THEN; IF i == 4 THEN CONTINUE ELIF i==8 THEN BREAK; VAR a = a + i; END
0
basic > a
[0, 1, 2, 3, 5, 6, 7]
basic > i
8
```

*TEST WHILE/BREAK/CONTINUE*

```
basic > VAR a = []
[]
basic > VAR i = 0  
0
basic > WHILE i < 10 THEN; VAR i = i + 1; IF i == 4 THEN CONTINUE; IF i==8 THEN BREAK; VAR a = a + i; END  
0
basic > a
[1, 2, 3, 5, 6, 7]
basic > i
8
```

**Syntax checks / Error handling**

**Note: RETURN can only be used in functions**

```
basic > RETURN
Invalid Syntax: RETURN can be used only within a function

File <stdin>, line 1

RETURN
      ^
```

**Note: BREAK and CONTINUE can only be used in loops**

```
basic > CONTINUE
Invalid Syntax: CONTINUE can be used only within a loop

File <stdin>, line 1

CONTINUE
        ^
basic > BREAK
Invalid Syntax: BREAK can be used only within a loop

File <stdin>, line 1

BREAK
     ^


```

### FUN - Functions

```
basic > FUN A()->1
<function A>

basic > FUN A()->1
<function A>

basic > FUN A(B,C,D)->B+C+D
<function A>
basic > A(1,2,3)
6


basic > FUN AFUNC(A, B, C); RETURN A + B * C END  
<function AFUNC>
basic > AFUNC(2,20,30)
602

basic > FUN AFUNC(A, B, C); VAR R = A + B * C ;;; RETURN R * R END 
<function AFUNC>
basic > AFUNC(10, 10, 39) 
160000

```

**Syntax checks / Error handling**

```
basic > FUN A
Invalid Syntax: Expected '('

File <stdin>, line 1

FUN A
     ^
basic > FUN A(
Invalid Syntax: Expected identifier or ')'

File <stdin>, line 1

FUN A(
      ^
basic > FUN A()
Invalid Syntax: Expected '->' or NEWLINE

File <stdin>, line 1

FUN A()
       ^


FUN A(B,C)=
          ^

basic > FUN A(B,C)=1
Invalid Syntax: Expected '->' or NEWLINE

File <stdin>, line 1

FUN A(B,C)=1
          ^

FUN A(B,C)=
          ^

basic > FUN A(B,C)=1
Invalid Syntax: Expected '->' or NEWLINE

File <stdin>, line 1

FUN A(B,C)=1
          ^

```

*Try to use the function name in an illegal manner*

```
basic > FUN A(B,C,D)->A+C+D 
<function A>
basic > A(1,2,3)
Traceback (most recent call last):
  File <stdin>, line 1, in <program>
Runtime Error: Illegal operation

FUN A(B,C,D)->A+C+D
              ^^^

```

*Test duplicate names*

```

basic > FUN A(A, B, C); VAR R = A + B * C ;;; RETURN R * R END
Invalid Syntax: Duplicate parameter 'A'. A parameter cannot share the same name as the function name

File <stdin>, line 1

FUN A(A, B, C); VAR R = A + B * C ;;; RETURN R * R END
      ^

basic > FUN A(b, c, A); VAR R = A + B * C ;;; RETURN R * R END
Invalid Syntax: Duplicate parameter 'A'. A parameter cannot share the same name as the function name

File <stdin>, line 1

FUN A(b, c, A); VAR R = A + B * C ;;; RETURN R * R END
            ^

basic > FUN test(A, test, C); VAR R = A * C ;;; RETURN R * R END
Invalid Syntax: Duplicate parameter 'test'. A parameter cannot share the same name as the function name

File <stdin>, line 1

FUN test(A, test, C); VAR R = A * C ;;; RETURN R * R END
            ^^^^
            ^

basic > FUN abc(A, test, test, T); VAR R = A * C ;;; RETURN R * R END
Invalid Syntax: Duplicate parameter 'test' in function definition

File <stdin>, line 1

FUN abc(A, test, test, T); VAR R = A * C ;;; RETURN R * R END
                 ^^^^
```

### example.myopl<br>
The file `example.myopl` contains a sample of the functionality of this language.<br>
The contents of the file is as follows:
```
# This is a very useful piece of software

FUN oopify(prefix) -> prefix + "oop"

FUN join(elements, separator)
	VAR result = ""
	VAR len = LEN(elements)

	FOR i = 0 TO len THEN
		VAR result = result + elements/i
		IF i != len - 1 THEN VAR result = result + separator
	END

	RETURN result
END

FUN map(elements, func)
	VAR new_elements = []

	FOR i = 0 TO LEN(elements) THEN
		APPEND(new_elements, func(elements/i))
	END

	RETURN new_elements
END

PRINT("Greetings universe!")

FOR i = 0 TO 5 THEN
	PRINT(join(map(["l", "sp"], oopify), ", "))
END
```

When run, the output is

```
basic > IMPORT "example.myopl"                                
Greetings universe!
loop, spoop
loop, spoop
loop, spoop
loop, spoop
loop, spoop
0


