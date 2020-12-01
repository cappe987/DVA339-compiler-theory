# part2

Generated lexer and parser using the libraries Alex and Happy.


## Desugaring? (Lecture 6)
Do a pre-pass to evaluator to rename variables to remove shadowing.
Keep track of display name for error messages.
I realize I've already similar to the renaming method in my interpreter, but without actually renaming anything.

Replace the types properly when desugaring? Not sure if this would give any advantage.


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

