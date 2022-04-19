    @R2
    M=0 // data initialize
    @R1
    D=M
    @COUNT
    M=D // set counter
(LOOP)
    @COUNT
    D=M
    @END
    D;JEQ // if @COUNT == 0 then goto (END)
    @COUNT
    M=M-1 // dec counter
    @R0
    D=M
    @R2
    M=M+D // @R2 = @R0 + @R2
    @LOOP
    0;JMP // goto (LOOP)
(END)
    @END
    0;JMP