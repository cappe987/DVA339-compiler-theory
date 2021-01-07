# Lab 2
This compiler was written as labs in the course DVA339 - Compiler Theory at MDH. It was written gradually over the time of the course, so code structure isn't the best as I wasn't able to plan for what was to come.

[Lab instructions](http://www.danielhedin.net/dva339/lab2/index.html)  
Lab 2.1: Generate a lexer and parser for the language  
Lab 2.2: Write a prettyprinter for the AST  
Lab 2.3: Write an interpreter for the language  
Lab 2.4: Write a typechecker for the language  
Lab 2.5: Write a compiler for the Trac42VM  
Lab 2.6: Write an optimizer for the language  

# Grammar of the language
```
Program ::= Decl*

Decl ::= Type id ( FormalList ) { Stmt* }
       | void id ( FormalList ) { Stmt* }

FormalList ::= Type id (, Type id)* | λ

Type ::= int | bool

Stmt ::= { Stmt* }
      |  if ( Expr ) Stmt
      |  if ( Expr ) Stmt else Stmt
      |  while ( Expr ) Stmt
      |  return Expr ;
      |  return ;
      |  Expr ;
      |  Type id ;

Expr ::= int
      |  true
      |  false
      |  Expr bop Expr
      |  id = Expr
      |  id
      |  uop Expr
      |  id ( ExprList )
      |  ( Expr )

ExprList ::= Expr (, Expr)* | λ
```

