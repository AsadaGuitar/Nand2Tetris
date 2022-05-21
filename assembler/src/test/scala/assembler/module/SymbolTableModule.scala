package assembler.module

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import assembler.data.AssemblyLine.{_, given}
import assembler.data.Mnemonic.{_, given}
import assembler.data.Symbol

import assembler.module.SymbolTableModule


class SymbolTableModuleSpec extends AnyFlatSpec with SymbolTableModule with Diagrams:

    /*
     *  TestType:
     *      All
     *  Function:
     *      SymbolTable#assignAddress(assembly: List[PassedInstruction]): List[AssignedInstruction]
     *  Description:
     *      Can be assigned to address assembler symbols.
     *  Data structure:
     *      sealed case class PassedLabel(line: String) extends PassedInstruction
     *      case class PassedA(variable: Int | String) extends PassedInstruction
     *      case class AssignedA(number: Int) extends AssignedInstruction
     *      case class AssignedC(dest: Dest, comp: Comp, jump: Jump) extends AssignedInstruction
     */
     "SymbolTable#assignAddress" should "Can be assigned to address assembler symbols." in {
        assert {
            val testData = List.empty[PassedInstruction]
            val expected = List.empty[PassedInstruction]
            expected === assignAddress(testData)
        }
        assert {
            val testData = List(PassedLabel("TEST"))
            val expected = List.empty[PassedInstruction]
            expected === assignAddress(testData)
        }
        assert {
            val testData = List(PassedA("100"))
            val expected = List(AssignedA(100))
            expected === assignAddress(testData)
        }
        assert {
            val testData = List(
                PassedA("R0"),
                PassedA("R9"),
                PassedA("R10"),
                PassedA("R15"),
                PassedA("R16")
            )
            val expected = List(
                AssignedA(0),
                AssignedA(9),
                AssignedA(10),
                AssignedA(15),
                AssignedA(1024)
            )
            expected === assignAddress(testData)
        }
        assert {
            val testData = List(
                PassedA("SP"),
                PassedA("THIS"),
                PassedA("SCREEN"),
                PassedA("KBD")
            )
            val expected = List(
                AssignedA(0),
                AssignedA(3),
                AssignedA(16384),
                AssignedA(24576)
            )
            expected === assignAddress(testData)
        }
        assert {
            val testData = List(
                PassedA("TEST01"),
                PassedA("TEST02"),
                PassedA("TEST01"),
                PassedA("TEST03")
            )
            val expected = List(
                AssignedA(1024),
                AssignedA(1025),
                AssignedA(1024),
                AssignedA(1026)
            )
            expected === assignAddress(testData)
        }
        assert {
            val testData = List(
                AssignedA(0),
                AssignedA(1024),
            )
            val expected = List(
                AssignedA(0),
                AssignedA(1024)
            )
            expected === assignAddress(testData)
        }
        assert {
            val testData = List(
                AssignedC(Dest.NULL, Comp.ZERO, Jump.NULL),
                AssignedC(Dest.PURE_D, Comp.D_PLUS_ONE, Jump.JMP),
            )
            val expected = List(
                AssignedC(Dest.NULL, Comp.ZERO, Jump.NULL),
                AssignedC(Dest.PURE_D, Comp.D_PLUS_ONE, Jump.JMP),
            )
            expected === assignAddress(testData)
        }
        assert {
            val testData = List(
                PassedLabel("TEST01"),
                PassedA("TEST01"),
                PassedLabel("TEST02"),
                PassedA("TEST02")
            )
            val expected = List(
                AssignedA(1),
                AssignedA(2)
            )
            expected === assignAddress(testData)
        }
        assert {
            val testData = List(
                PassedLabel("TEST01"),
                PassedA("TEST02"),
                AssignedC(Dest.PURE_D, Comp.D_PLUS_ONE, Jump.JMP),
                PassedA("TEST03"),
                AssignedC(Dest.PURE_M, Comp.D_MINUS_A, Jump.JEQ),
                PassedLabel("TEST04"),
                PassedA("TEST01"),
                AssignedC(Dest.NULL, Comp.D_AND_M, Jump.JGT),
                PassedA("TEST04"),
                AssignedC(Dest.NULL, Comp.ZERO, Jump.JMP)
            )
            val expected = List(
                AssignedA(1024),
                AssignedC(Dest.PURE_D, Comp.D_PLUS_ONE, Jump.JMP),
                AssignedA(1025),
                AssignedC(Dest.PURE_M, Comp.D_MINUS_A, Jump.JEQ),
                AssignedA(1),
                AssignedC(Dest.NULL, Comp.D_AND_M, Jump.JGT),
                AssignedA(5),
                AssignedC(Dest.NULL, Comp.ZERO, Jump.JMP)
            )
            expected === assignAddress(testData)
        }
     }
     