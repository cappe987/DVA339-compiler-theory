# Lab 1
A lexer and parser for the language below.

# Language (not in LL(1) form)
S -> S; S 
S -> id := E 
S -> print ( L )

E -> id 
E -> num 
E -> E + E 
E -> ( S, E )

L -> E 
L -> L, E

