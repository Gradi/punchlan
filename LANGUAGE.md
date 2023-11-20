# Punchlan description

Program written in Punchlan consists of **1 to more** source code files. One file is denoted as an entry point.
Source files has `.pl` extension.

`punchc` accepts single source file as entry point, rest files will be located automatically.

Single source file consists of:

- Open directives;
- Declarations
  - Global variable declaration
  - Function declaration
  - Type declaration
- Own filename


### Open directives

- Implicit
```
open libc/stdio
open punch
```
- Aliased
```
open libc/stdio as coolAliasName
open something as somethingElse
```

Note  a forward slash `/`. It divides paths into directories and filenames.
For example, consider following file structure tree:

```
punch.pl
    libc/stdio.pl
    libc/stdlib.pl
```

Then:

- `open punch` references `./punch.pl` file;
- `open libc/stdio.pl` references `./libc/stdlib.pl` file;

Standard library is located at the same location where `punchc` is located, in subdirectory named `stdlibrary`.
Thus, if file is not found in current working directory, compiler will look for it in `<punchc_location>/stdlibrary`.

### Variable declaration

Variable is declared like this: `'var' IDENTIFIER ':' TYPEID ['=' EXPRESSION]'`. It, optionally, can be prefixed with
`extern`, `export` keywords.

```
var hello: const pointer<const char> = "Hello World"
var number: int32 = 123123
var bssVariable : int32
```

### Function declaration

Function is declared like this:
```
['extern' | 'export'] func IDENTIFIER '(' FUNC_ARGS ')' [ : TYPEID ]
    STATEMENTS
endfunc
```

For example
```
func sum(a: int32, b: int32) : int32
    return a + b
endfunc

export func main ()
    var a : int32 = 5
    var b : int32 = 6
    var sum : int32 = add(a, b)
    puts ("Hello World!")
endfunc
```

If return type is omitted, it defaults to `void` (eg. function returns nothing)

#### Statements

```
STATEMENTS = STATEMENT NEWLINE STATEMENTS
STATEMENTS =

STATEMENT = IDENTIFIER ':' TYPEID [ '=' EXPRESSION ] //  Variable declaration
STATEMENT = IDENTIFIER '=' EXPRESSION // Variable assignment
STATEMENT = 'if' EXPRESSION 'then' STATEMENTS ['elseif' EXPRESSION 'then' STATEMENTS]* ['else' STATEMENTS ] 'fi' // Ifs
STATEMENT = 'for' IDENTIFIER 'in' EXPRESSION '..' EXPRESSION '..' [ '..' EXPRESSION ] 'do' STATEMENTS 'endfor' // For
STATEMENT = 'while' EXPRESSION 'do' STATEMENTS 'endwhile' // While
STATEMENT = 'defer' STATEMENTS 'enddefer' // Defer
STATEMENT = 'return'
STATEMENT = 'return' EXPRESSION
STATEMENT = EXPRESSION
```

This is unformal description.

`derer`'s are executed after function body.

#### Expressions

```
EXPRESSION = CONST
EXPRESSION = IDENTIFIER
EXPRESSION = IDENTIFIER '(' FUNC_ARGS ')'
EXPRESSION = EXPRESSION '.' IDENTIFIER
EXPRESSION = EXPRESSION '[' EXPRESSION ']'
EXPRESSION = BINARYEXPRESSION
EXPRESSION = IDENTIFIER '{' FIELDS '}'
EXPRESSION = '~' EXPRESSION
EXPRESSION = 'sizeof' '(' TYPEID ')'
EXPRESSION = 'addrof' '(' EXPRESSION ')'
EXPRESSION = 'deref' '(' EXPRESSION ')'
EXPRESSION = 'cast' '(' TYPEID ',' EXPRESSION ')'

BINARYEXPRESSION = EXPRESSION OP EXPRESSION
OP = '+'
OP = '-'
OP = '*'
OP = '/'
OP = '=='
OP = '!='
OP = '<'
OP = '<='
OP = '>'
OP = '>='
OP = 'or'
OP = 'and'
OP = 'xor'
OP = '<<'
OP = '>>'
```

### Type declaration

There can be `structs`'s and `union`'s. Like in C.

```
struct IDENTIFIER NEWLINE
    IDENTIFIER ':' TYPEID
    .
    .
    .
    IDENTIFIER ':' TYPEID
```

For example,

```
struct Person
    Name: pointer<const char>
    Age: int32
    IsFlagActive: bool
endstruct

union Foo
    valuef: double
    valuei: int32
    bool: bool
endunion
```

### Type ID

Type id is defined like this:

```
TYPEID =
    |  'int8' | 'uint8'
    | 'int16' | 'uint16'
    | 'int32' | 'uint32'
    | 'int64' | 'uint64'
    | 'float'
    | 'double'
    | 'bool'
    | 'char'
    | 'void'
    |  'pointer' '<' TYPEID '>'
    | 'const' TYPEID
    | IDENTIFIER [ '.' IDENTIFIER ]
```

Basically, this is all language.
