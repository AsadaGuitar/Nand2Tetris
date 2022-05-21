@COUNT
M = 1
@100
D = A
@MAX // size 
M = D
(LOOP)
// loop function
@COUNT
D=M
D=D+1
@RESULT
M=D
@LOOP
D;JEQ
@END
0;JMP
(END)
// infiniry loop
@END
0;JMP

