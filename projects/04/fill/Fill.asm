// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

    @8192
    D=A
    @FULL_SCREEN
    M=D
    @COUNT
    M=0
(LOOP)
    @KBD
    D=M
    @PAINT_DISPLAY
    D;JNE
    @COUNT
    D=M
    @LOOP
    D;JEQ
    @DELETE_DISPLAY
    0;JMP

(PAINT_DISPLAY)
    @SCREEN
    D=A
    @COUNT
    D=D+M
    @FULL_SCREEN
    A=M-D
    @LOOP
    A;JEQ
    A=D
    M=-1
    @COUNT
    M=M+1
    @LOOP
    0;JMP

(DELETE_DISPLAY)
    @SCREEN
    D=A
    @COUNT
    M=M-1
    D=D+M
    A=D
    M=0
    @LOOP
    0;JMP