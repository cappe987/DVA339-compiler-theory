# part1





# Original from Lab 1.1 (not LL(1))
S -> S; S 
S -> id := E 
S -> print ( L )

E -> id 
E -> num 
E -> E + E 
E -> ( S, E )

L -> E 
L -> L, E

