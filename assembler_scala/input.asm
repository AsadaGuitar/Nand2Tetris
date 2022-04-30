@count
M = 0
D = M
@max
M = 1
(LOOP)
@count
M = M + 1
D = M
@max
D = A - D
@LOOP
D; JEQ
@END
0; JMP
(END)
@END
0;JMP
