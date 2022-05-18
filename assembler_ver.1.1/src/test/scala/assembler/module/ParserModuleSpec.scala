package assembler.module

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import assembler.module.ParserModule

class ParserModuleSpec extends AnyFlatSpec with ParserModule with Diagrams with PrivateMethodTester:

  "moldAssemblyParser" should "can padding assembly code." in {
    assert(moldAssembly(Iterable.empty[String]) === Iterable.empty[String])
    assert(
      moldAssembly(Seq(" abcd", "e f g", "//hij", "klm//", "//", "mop//qr")) ===
      Iterable("abcd", "efg", "klm", "mop")  
    )
  }

  /**
   * labelParser: 'success pattern
   */
  "labelParser" should "can extract the jump destination of hack assembler." in {
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
  it should "fails if not enclosed in parentheses or if the first character is a number." in {
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
   * instructionAParser: 'success pattern
   */
  "instructionAParser" should "can extract 'A instruction' of hack assembler." in {
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
   * instructionAParser: 'failure pattern
   */
  it should "fails if leading @ is not present or if a number is entered after @." in {
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
   * instructionCParser: 'success pattern
   */
  "instructionCParser" should "can extract 'C instruction' of hack assembly." in {
    def test(data: String) = assert(parseAll(instructionCParser, data).successful)
    test("M=-A;null")
    test("null=0;JMP")
    test("AMD=M-1;JLT")
    test("A=D|M")
    test("MD=A")
    test("null=M-D")
    test("AM=0")
    test("D=!D")
    test("0;JMP")
    test("M;null")
    test("A-1;JEQ")
    test("D+M;JGT")
  }

  /**
   * instructionCParser: 'failure pattern
   */
  it should "fails when a non mnemonic C instruction is received." in {
    def test(data: String) = assert(!parseAll(instructionCParser, data).successful)
    test("")
    test(" ")
    test(" = ;")
    test("B=A;null")
    test("M=+D;JMP")
    test("M=-D;TEST")
    test(" =-D;JMP")
    test("AMD= ;JEQ")
    test("A=A+1; ")
    test(" = ")
    test("D= ")
    test(" =M")
    test("E=A+1")
    test("A=E")
    test("MD=10")
    test("D=D|1")
    test("D=M&0")
    test(" ; ")
    test("A+1;JUMP")
  }