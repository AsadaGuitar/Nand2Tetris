package assembler.module

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import assembler.module.ParserModule

class ParserModuleSpec extends AnyFlatSpec with ParserModule with Diagrams with PrivateMethodTester:
  /**
   * labelParser: 'success pattern
   */
  "labelParser" should "Can extract the jump destination of hack assembler." in {
    def test(data: String) = assert(parseAll(labelParser, data).successful)
    test("(Hello)")
    test("(Hello5)")
    test("(He5llo)")
    test("(:Hel_lo.)")
    test("(_He.llo:)")
    test("(.Hel:lo_)")
  }
  /**
   * labelParser: 'failure pattern
   */
  it should "Fails if not enclosed in parentheses or if the first character is a number." in {
    def test(data: String) = assert(!parseAll(labelParser, data).successful)
    test("")
    test(" ")
    test("HELLO")
    test("(HELLO")
    test("HELLO)")
    test("()")
    test("(123)")
    test("(123HELLO)")
    test("((HELLO))")
  }

  /**
   * commandAParser: 'success pattern
   */
  "commandAParser" should "Can extract 'A instruction' of hack assembler." in {
    def test(data: String) = assert(parseAll(instructionAParser, data).successful)
    test("@Hello")
    test("@hello5")
    test("@Hel5lo")
    test("@.HEL:LO_")
    test("@:He_llo.")
    test("@_hel.lo:")
    test("@123")
  }
  /**
   * commandAParser: 'failure pattern
   */
  it should "Fails if leading @ is not present or if a number is entered after @." in {
    def test(data: String) = assert(!parseAll(instructionAParser, data).successful)
    test("")
    test(" ")
    test("@")
    test("HELLO")
    test("@5hello")
    test("@He@llo")
    test("He@llo")
    test("@@")
  }

  /**
   * commandCParser: 'success pattern
   */
  "commandCParser" should "Can extract 'C instruction' of hack assembly." in {
    def test(data: String) = assert(parseAll(instructionCParser, data).successful)
    // dest=comp;jump
    test("M=-A;null")
    test("null=0;JMP")
    test("AMD=M-1;JLT")
    // dest=comp
    test("A=D|M")
    test("MD=A")
    test("null=M+A")
    test("AM=0")
    test("D=!D")
    // comp;jump
    test("0;JMP")
    test("M;null")
    test("A&M;JEQ")
    test("D+M;JGT")
  }

  it should "Fails when a non mnemonic C instruction is received." in {
    def test(data: String) = assert(!parseAll(instructionCParser, data).successful)
    // empty
    test("")
    // whitespace
    test(" ")
    // dest=comp;jump
    test(" = ;")
    test("B=A;null")
    test("M=+D;JMP")
    test("M=-D;TEST")
    test(" =-D;JMP")
    test("AMD= ;JEQ")
    test("A=A+1; ")
    // dest=comp
    test(" = ")
    test("D= ")
    test(" =M")
    test("E=A+1")
    test("A=E")
    test("MD=10")
    test("D=D|1")
    test("D=M&0")
    // comp;jump
    test(" ; ")
    test("A+1;JUMP")
  }