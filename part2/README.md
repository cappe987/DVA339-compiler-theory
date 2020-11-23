# part2

Generated lexer and parser using the libraries Alex and Happy.

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