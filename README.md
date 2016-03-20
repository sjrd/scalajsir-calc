# Scala.js IR-based Calculator

This repository is a toy project for developers who want to get familiar with the Scala.js IR.
It contains the necessary architecture for parsing small expressions, compiling them to IR trees, then link and executes those trees.

It is *intentionally not complete*.
The critical part of compiling a math expression tree into a typed IR tree is left out.
That part is meant to be filled in as an exercise to get familiar with the IR.

**Please do *not* look at open Pull Requests of this repository.**
They will contain ongoing work from other developers and comments from Scala.js authors.
Looking at those solutions defeats the purpose of *you* doing the work to learn!

## Get started

Open `sbt` and run the application with:

    > run "5"
    
which will run the calculator with the expression `5`.
After a bit of logging, this should eventually print the number `5` on the console.

If you try with anything else but a number, as

    > run "5 + 4"
    
You will get a run-time exception, because the compiler cannot handle anything else.

Your task is to fill in the `Compiler.compileExpr` method to make it support other kinds of expressions.

## Unit tests

There is a small infrastructure for unit-testing your compiler, in the `src/test/scala` directory.
You can run the unit tests with

    > test
    
## The language supported by the parser

* There are two types in the language: `number` and `function`.
* `number` is equivalent to a `Double` in Scala.
* The parameters of functions are always numbers, and they always return numbers.
  There is no higher-order function in this language.
  Functions are therefore only further typed by their *arity*, i.e., the number of parameters they take.
* Functions can capture `let` bindings and parameters from their enclosing function.
  Captures can be numbers of functions.

The existing parser recognizes the following language.
`[...]` denotes an optional production, and `{...}` a zero-to-many production.

    expr ::= ifThenElse
           | let
           | fun
           | addSub
    
    ifThenElse ::= 'if' '(' expr ')' addSub 'else' addSub
      
      The condition must be a number. It is "truthy" if it is non-0.
      The two branches must have the same type.
    
    let ::= 'let' identifier '=' expr 'in' expr
    
    fun ::= 'fun' '(' [identifier {',' identifier}] ')' '=' '{' expr '}'
    
    addSub ::= divMul { ('+' | '-') divMul }
    
    divMul ::= factor { ('*' | '/') factor }
    
    factor ::= base { '(' [expr {',' expr}] ')' }
    
      Parentheses denote function call.
      For calls, `base` must be a function type with the correct arity.
      
    base ::= literalNumber
           | identifier
           | '(' expr ')'
           
      An identifier can refer to a lexically enclosing `let` binding or
      function parameter.
      
## Task, in order of difficulty

You should implement (parts of) the above language.
The recommended order is the following:

1. Binary operators
2. Let bindings
3. if/then/else
4. Lambda declaration (without captures) + calls
5. Add common math function in the initial environment
   (such as `sin`, `log`, and others found in `java.lang.Math`)
6. Support captures in lambdas.

While you do this, it is encouraged that you define unit tests for your compiler.

## Submitting a Pull Request (PR)

Once you get something working, you can open a pull request with your work, to receive comments.
On the pull request discussion, you should act as a contributor would to get their work in the repository.
However, in reality, your pull request will *not* be merged, since that would prevent other developers to go through the same learning process.

It is *not* necessary to complete the entire language to open a PR.
In fact, you can do so as early as you wish to get feedback.

This repository is a learning tool.

## Going further

For fun, you are free to expand the language, its parser and its compiler, but it is not expected that you do so.

## Coding style and contributing guide

As this repository is meant as a learning tool for developing Scala.js, it follows the same coding style and contributing guidelines.
See the master repository at https://github.com/scala-js/scala-js

## License

The code of this repository is distributed under the MIT license:

> Copyright (c) 2016 Sébastien Doeraene
>
> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
>
> The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE
