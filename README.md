# B3TC Lab 3

This directory contains a compiler for a subset of C#.

The target language is the *Simple Stack Machine* (SSM),
a virtual machine with a graphical simulator.

You can run the graphical simulator by calling `ssm.sh`,`ssm2.sh`,`ssm.bat`, or `ssm2.bat`. Which you should call depends on your OS (`.bat` on windows) and your Java version.
If none of those work, there is also an online version: https://ssm.asraphiel.dev/.
It is made and maintained by an ex-student over at https://github.com/J00LZ/ssmrs, and while we can't promise anything, we've only seen one bug in it, which seems to be fixed now.

## Tasks

1. (0 cp)
    Get familiar with the features the compilers already supports, and the testing framework.

2. (20 cp)
    Fix the priorities of the operators. In the starting framework, all
    operators have the same priority. Use the official C# reference to determine the correct order of operations:

    * <https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/>
    * <https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions>

3. (10 cp)
    In the lexer, discard C# single-line comments.

4. (10 cp)
    Make the parser handle both left-associative and right-associative operators.
  
    Again, use the official language reference to determine what the associativity of each operator should be!

    This means that, e.g.
  
    * `a = b = 1` should be parsed as `a = (b = 1)` (right-associative)
    * `a + b + c` should be parsed as `(a + b) + c` (left-associative)

5. (20 cp)
    Extend the compiler to support a `for` statement.
    In particular, we extend the grammar as such:

    ```bnf
    stat ::=  ...
          |   for ( exprdecls? ; expr ; exprdecls? ) stat
    exprdecls ::= exprdecl | exprdecl , exprdecls
    exprdecl  ::= expr | decl
    ```

    This lets us write (e.g.) the following loop:

    ```csharp
    for(int i, i = 0; i<5; i = i + 1){}
    ```

    Make sure that the generated code behaves as expected.

    Hint: you may find generating code for `for` loops easier if you first translate
    their abstract syntax to use `while` loops (this is called 'desugaring').

6. (40 cp)
    Adapt the code generator such that the declared local variables can be used.

    Hint: you should change the result type for statements in the algebra to be a *pair* of two things:
    the generated code, *plus* a list of any variables declared.

    Remember that in C# local variables can be declared anywhere in a method body,
    but they *always* must be declared *before use*.
    So, in a sequence of statements, the environment passed to a statement must contain all variables declared before it.

7. (20 cp)
    Change the code generator for the logical operators, so that they are computed lazily, as is usual in C#.

    In other words, the right operand should only be evaluated if necessary to determine the result.

8. (30 cp)
    Add the possibility to 'call a method with parameters' to the syntax of expressions,
    and add the possibility to deal with such calls to the rest of the compiler.

    Make sure that the parameters can be used within the function body.

    Hint: In the `codeAlgebra`, you have to change the result types.
    You need to pass around an environment that contains the addresses of parameters.

9. (10 cp)
    Add a special case for a method call to `print` which,
    instead of jumping to the label "`print`",
    evaluates its argument(s) and does `TRAP 0` for each argument.

    The command `TRAP 0` will pop and print the topmost element from the stack.

    For example,

    ```csharp
    print(2+3)
    ```

    should be compiled to

    ```
    LDC 2
    LDC 3
    ADD
    TRAP 0
    ```

    and

    ```csharp
    print(1,2)
    ```

    should first print `1`, and then print `2`.

    Note: if you do not implement this exercise, expect to miss out on far more than 10cp,
    since the autograder uses `print` statements extensively for testing.

10. (20 cp)
    Extend the code generator such that methods can have a result.

    You may choose whether you want to pass the result via register
    or via the stack.

11. (10 cp)
    Modify the compiler to fail at compile-time if the input contains
    any references to undefined variables.

    (This is called *scope-checking*)

    The compiler should fail with exit code 21 for this error.

12. (10 cp)
    Modify the compiler to fail at compile-time if the input contains
    any assignments to variables of the wrong type.

    (This is called *type-checking*)

    The compiler should fail with exit code 22 for this error.

13. (10 cp)
    Modify the compiler to fail at compile-time if the input contains
    any function calls with the wrong number of arguments.

    The compiler should fail with exit code 23 for this error.

14. (10 cp)
    Modify the compiler to fail at compile-time if the input contains
    any function calls with arguments of the wrong type.

    The compiler should fail with exit code 24 for this error.

15. (10 cp)
    Modify the compiler to fail at compile-time if the input contains
    any other syntax (e.g. operators, `if` statements, etc.) with arguments of the wrong type.

    The compiler should fail with exit code 25 for this error.

15. (20 cp)
    Modify the code generator such that declared member variables can be used.

    Our C# programs consist of exactly one `Class`,
    which means that these are global variables.

## Compiler Notes

### Parsing

The file `src/Parser.hs` contains a parser for our subset of C#.

#### Beware `greedyChoice`!

The keyword lexer must occur before `lexLowerId`, because a keyword such as "`class`"
or a type such as "`int`" could also be interpreted as lowercase identifiers.

For similar reasons, `OpLeq` must occur before `OpLt` in `AbstratSyntax.Operator`,
otherwise the string "`<=`" might be interpreted as the operator `<` followed by the operator `=`
rather than as a single operator.

### Simple Stack Machine

To simplify generation of target code for the simple stack machine, 
the file `src/SSM.hs` defines an abstract syntax tree for SSM code.

This allows the generation of SSM code to be split into two parts:

1. Generation of SSM abstract syntax (complex code, but type-checked by Haskell)
2. Printing out of arbitrary SSM abstract syntax (simple, repetitive code defined alongside the AST)

The structure of an SSM program is simple -- it is a list of instructions.

### Acknowledgements

This assignment is heavily inspired and to a large extent copied from an
assignment Johan Jeuring has been using.

## Submission Instructions

* Do not change any of the type signatures present in the template,
  and only change the data type definitions that are currently empty
  (such as `Token` or `Ident`).

* Ensure you fully understand your code & can answer questions about it in an oral exam.

    You will find this easier if you write your code readably and in an idiomatic Haskell style, e.g.

    * Include useful comments in your code.
        Do not paraphrase the code, but describe the general structure, special cases, preconditions, invariants, etc.
    * Use existing higher-order functions (e.g. `map`, `foldr`, `filter`, `zip`).
    * Use existing libraries (the cabal file lists what is available)

* Read through and be bound by the LICENSE.txt file

    Note in particular the **absolute ban** on using LLM or 'AI' tools, that **you are responsible** for the authenticity & confidentiality of your submission, and the **dire penalties** for trying to cheat.

* Submit through Brightspace
