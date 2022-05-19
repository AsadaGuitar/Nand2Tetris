package assembler.module

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import assembler.data.AssemblyLine.{_, given}
import assembler.data.Mnemonic.{_, given}
import assembler.data.Symbol.{_, given}

import assembler.module.ParserModule


class ParserModuleSpec extends AnyFlatSpec with ParserModule with Diagrams with PrivateMethodTester:

  /**
   *  TestType:
   *    All
   *  Name: 
   *    moldAssemblyParser
   *  Args:
   *    Iterable[String]
   *  Returns:
   *    Iterable[String]
   *  Description:
   *    Remove whitespace and comments from all strings.
   */
  "moldAssemblyParser" should "can padding assembly code." in {
    assert(moldAssembly(Iterable.empty[String]) === Iterable.empty[String])
    assert{
      val testData = Seq(" abcd", "e f g", "//hij", "klm//", "//", "mop//qr")
      moldAssembly(testData) === Iterable("abcd", "efg", "klm", "mop")  
    }
  }

  /**
   *  TestType:
   *    Success
   *  Name: 
   *    labelParser
   *  Returns:
   *    Parser[PassedLabel]
   *  Description:
   *    Parser that can extract '(Label)' from Hack assembler.
   *  Data structures:
   *    sealed case class PassedLabel(line: String) extends PassedInstruction
   */
  "labelParser" should "can extract the jump destination of hack assembler." in {
    assert{
      val r = parseAll(labelParser, "(TEST)")
      if r.successful then r.get.line === "TEST"
      else false
    }
    assert{
      val r = parseAll(labelParser, "(TE1ST)")
      if r.successful then r.get.line === "TE1ST"
      else false
    }
    assert{
      val r = parseAll(labelParser, "(:TE_ST.)")
      if r.successful then r.get.line === ":TE_ST."
      else false
    }
    assert{
      val r = parseAll(labelParser, "(_TE.ST:)")
      if r.successful then r.get.line === "_TE.ST:"
      else false
    }
    assert{
      val r = parseAll(labelParser, "(.TE:ST_)")
      if r.successful then r.get.line === ".TE:ST_"
      else false
    }
  }
  
  /**
   *  TestType:
   *    Failure
   *  Name: 
   *    labelParser
   *  Returns:
   *    Parser[PassedLabel]
   *  Description:
   *    Parser that can extract '(Label)' from Hack assembler.
   *  Data structures:
   *    sealed case class PassedLabel(line: String) extends PassedInstruction
   */
  it should "fails if not enclosed in parentheses or if the first character is a number." in {
    assert(!parseAll(labelParser, "").successful)
    assert(!parseAll(labelParser, " ").successful)
    assert(!parseAll(labelParser, "TEST").successful)
    assert(!parseAll(labelParser, "(TEST").successful)
    assert(!parseAll(labelParser, "TEST)").successful)
    assert(!parseAll(labelParser, "(123)").successful)
    assert(!parseAll(labelParser, "(@TEST)").successful)
    assert(!parseAll(labelParser, "((TEST))").successful)
    assert(!parseAll(labelParser, "(TEST))").successful)
    assert(!parseAll(labelParser, "((TEST)").successful)
  }

  /**
   *  TestType:
   *    Success
   *  Name: 
   *    instructionAParser
   *  Returns:
   *    Parser[PassedA]
   *  Description:
   *    Parser that can extract '@Symbol' or '@Number' from Hack assembler.
   *  Data structures:
   *    case class PassedA(variable: Int | String) extends PassedInstruction
   */
  "instructionAParser" should "can extract 'A instruction' of hack assembler." in {
    assert{
      val r = parseAll(instructionAParser, "@TEST")
      if r.successful then r.get.variable === "TEST"
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@TE1ST")
      if r.successful then r.get.variable === "TE1ST"
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@.HEL:LO_")
      if r.successful then r.get.variable === ".HEL:LO_"
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@:He_llo.")
      if r.successful then r.get.variable === ":He_llo."
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@_hel.lo:")
      if r.successful then r.get.variable === "_hel.lo:"
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@123")
      if r.successful then r.get.variable === 123
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@R0")
      if r.successful then r.get.variable === 0
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@9")
      if r.successful then r.get.variable === 9
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@R10")
      if r.successful then r.get.variable === 10
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@R15")
      if r.successful then r.get.variable === 15
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@R16")
      if r.successful then r.get.variable === "R16"
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@SP")
      if r.successful then r.get.variable === 0
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@ARG")
      if r.successful then r.get.variable === 2
      else false
    }
    assert{
      val r = parseAll(instructionAParser, "@SCREEN")
      if r.successful then r.get.variable === 16384
      else false
    }
  }

  /**
   *  TestType:
   *    Failure
   *  Name: 
   *    instructionAParser
   *  Returns:
   *    Parser[PassedA]
   *  Description:
   *    Parser that can extract '@Symbol' or '@Number' from Hack assembler.
   *  Data structures:
   *    case class PassedA(variable: Int | String) extends PassedInstruction
   */
  it should "fails if leading @ is not present or if a number is entered after @." in {
    def test(data: String) = assert(!parseAll(instructionAParser, data).successful)
    assert(!parseAll(instructionAParser, "").successful)
    assert(!parseAll(instructionAParser, " ").successful)
    assert(!parseAll(instructionAParser, "@").successful)
    assert(!parseAll(instructionAParser, "TEST").successful)
    assert(!parseAll(instructionAParser, "@123TEST").successful)
    assert(!parseAll(instructionAParser, "@TE@ST").successful)
    assert(!parseAll(instructionAParser, "TE@ST").successful)
    assert(!parseAll(instructionAParser, "@@").successful)
  }

  /**
   *  TestType:
   *    Successs
   *  Name: 
   *    instructionCParser
   *  Returns:
   *    Parser[AssignedC]
   *  Description:
   *    Parser that can extract assembler's mnemonic from Hack assembler.
   *  Data structures:
   *    case class AssignedC(dest: Dest, comp: Comp, jump: Jump) extends AssignedInstruction
   */
  "instructionCParser" should "can extract 'C instruction' of hack assembly." in {
    assert {
      val testData = "null=0;null"
      val expected = AssignedC(Dest.NULL, Comp.ZERO, Jump.NULL)
      val r = parseAll(instructionCParser, testData)
      if r.successful then r.get === expected
      else false
    }
    assert {
      val testData = "AMD=D|A;JMP"
      val expected = AssignedC(Dest.PURE_AMD, Comp.D_OR_A, Jump.JMP)
      val r = parseAll(instructionCParser, testData)
      if r.successful then r.get === expected
      else false
    }
    assert {
      val testData = "MD=A+1"
      val expected = AssignedC(Dest.PURE_MD, Comp.A_PLUS_ONE, Jump.NULL)
      val r = parseAll(instructionCParser, testData)
      if r.successful then r.get === expected
      else false
    }
    assert {
      val testData = "M-D;JLT"
      val expected = AssignedC(Dest.NULL, Comp.M_MINUS_D, Jump.JLT)
      val r = parseAll(instructionCParser, testData)
      if r.successful then r.get === expected
      else false
    }
    assert {
      val testData = "!M"
      val expected = AssignedC(Dest.NULL, Comp.NOT_M, Jump.NULL)
      val r = parseAll(instructionCParser, testData)
      if r.successful then r.get === expected
      else false
    }
    assert {
      val testData = "A=A"
      val expected = AssignedC(Dest.PURE_A, Comp.PURE_A, Jump.NULL)
      val r = parseAll(instructionCParser, testData)
      if r.successful then r.get === expected
      else false
    }
  }

  /**
   *  TestType:
   *    Failure
   *  Name: 
   *    instructionCParser
   *  Returns:
   *    Parser[AssignedC]
   *  Description:
   *    Parser that can extract assembler's mnemonic from Hack assembler.
   *  Data structures:
   *    case class AssignedC(dest: Dest, comp: Comp, jump: Jump) extends AssignedInstruction
   */
  it should "fails when a non mnemonic C instruction is received." in {
    assert(!parseAll(instructionCParser, "").successful)
    assert(!parseAll(instructionCParser, " ").successful)
    assert(!parseAll(instructionCParser, "=;").successful)
    assert(!parseAll(instructionCParser, " = ; ").successful)
    assert(!parseAll(instructionCParser, "TEST=TEST;TEST").successful)
    assert(!parseAll(instructionCParser, "TEST=A;JMP").successful)
    assert(!parseAll(instructionCParser, "A=TEST;JMP").successful)
    assert(!parseAll(instructionCParser, "A=A;TEST").successful)
    assert(!parseAll(instructionCParser, " =A;JMP").successful)
    assert(!parseAll(instructionCParser, "A= ;JMP").successful)
    assert(!parseAll(instructionCParser, "A=A; ").successful)
    assert(!parseAll(instructionCParser, "D=M&0").successful)
    assert(!parseAll(instructionCParser, "null=A|M;JMP").successful)
    assert(!parseAll(instructionCParser, "D=M+D;JMP").successful)
  }
  