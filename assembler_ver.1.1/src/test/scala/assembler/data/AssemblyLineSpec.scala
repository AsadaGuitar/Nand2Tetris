package assembler.data

import cats._
import cats.data._
import cats.implicits._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import assembler.data.Mnemonic.{_, given}
import assembler.data.AssemblyLine.{_, given}

import lib.syntax.IntSyntax.{_, given}


class AssemblyLineSpec extends AnyFlatSpec with Diagrams  with PrivateMethodTester:

    /**
     *  TestType:
     *    All
     *  Function: 
     *    AssignedA#binary(using BinaryConvertor[Int]): Seq[Boolean]
     *  Description:
     *    Convert assigned A instruction's number to binary.
     */
    "AssignedA#binary" should "can convert Int to Seq[Boolean]" in {
        assert(AssignedA(0).binary === Seq(false))
        assert(AssignedA(2).binary === Seq(true, false))
        assert(AssignedA(10).binary === Seq(true, false, true, false))
    }

    /**
     *  TestType:
     *    All
     *  Function: 
     *    AssignedC#binary: Seq[Boolean]
     *  Description:
     *    Convert assigned C instruction's mnemonics to binary.
     */
    "AssignedC#binary" should "can convert Dest, Comp, Jump to Seq[Boolean]" in {
        assert {
            val test = AssignedC(Dest.NULL, Comp.ZERO, Jump.NULL).binary
            val expected = Seq(
                true, true, true,
                false, false, false, 
                false, true, false, true, false, true, false,
                true, true, true
            )
            test === expected
        }
        assert {
            val test = AssignedC(Dest.NULL, Comp.ZERO, Jump.JMP).binary
            val expected = Seq(
                true, true, true,
                false, false, false, 
                false, true, false, true, false, true, false,
                true, true, false
            )
            test === expected
        }
        assert {
            val test = AssignedC(Dest.NULL, Comp.A_MINUS_D, Jump.NULL).binary
            val expected = Seq(
                true, true, true,
                false, false, false, 
                false, false, false, false, true, true, true,
                true, true, true
            )
            test === expected
        }
        assert {
            val test = AssignedC(Dest.NULL, Comp.A_PLUS_ONE, Jump.JMP).binary
            val expected = Seq(
                true, true, true,
                false, false, false, 
                false, true, true, false, true, true, true, 
                true, true, false
            )
            test === expected
        }
        assert {
            val test = AssignedC(Dest.PURE_MD, Comp.ZERO, Jump.NULL).binary
            val expected = Seq(
                true, true, true,
                false, true, true, 
                false, true, false, true, false, true, false,
                true, true, true
            )
            test === expected
        }
        assert {
            val test = AssignedC(Dest.PURE_M, Comp.ZERO, Jump.JLT).binary
            val expected = Seq(
                true, true, true,
                false, false, true, 
                false, true, false, true, false, true, false,
                false, true, true
            )
            test === expected
        }
        assert {
            val test = AssignedC(Dest.PURE_A, Comp.M_MINUS_D, Jump.JGT).binary
            val expected = Seq(
                true, true, true,
                true, false, false, 
                true, false, false, false, true, true, true,
                false, false, false
            )
            test === expected
        }
    }
