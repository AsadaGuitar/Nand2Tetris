package assembler.data

import cats._
import cats.data._
import cats.implicits._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import assembler.data.Mnemonic.{_, given}
import assembler.data.AssemblyLine._

class AssemblyLineSpec extends AnyFlatSpec with Diagrams  with PrivateMethodTester:

    "AssignedA#binary" should "can convert Int to Seq[Boolean]" in {
        assert(AssignedA(0).binary === Seq(false))
        assert(AssignedA(2).binary === Seq(true, false))
        assert(AssignedA(10).binary === Seq(true, false, true, false))
    }

    // 16bit : Seq(false, false, false, false,  false, false, false, false
    //              false, false, false, false,  false, false, false, false)
    "AssignedC#binary" should "can convert Option[Dest], Comp, Option[Jump] to Seq[Boolean]" in {
        assert {
            val test = AssignedC(None, Comp.ZERO, None).binary
            val expected = Seq(
                true, true, true,
                false, false, false, 
                false, true, false, true, false, true, false,
                true, true, true
            )
            test === expected
        }
        assert {
            val test = AssignedC(None, Comp.ZERO, Some(Jump.JMP)).binary
            val expected = Seq(
                true, true, true,
                false, false, false, 
                false, true, false, true, false, true, false,
                true, true, false
            )
            test === expected
        }
        assert {
            val test = AssignedC(None, Comp.A_MINUS_D, None).binary
            val expected = Seq(
                true, true, true,
                false, false, false, 
                false, false, false, false, true, true, true,
                true, true, true
            )
            test === expected
        }
        assert {
            val test = AssignedC(None, Comp.A_PLUS_ONE, Some(Jump.JMP)).binary
            val expected = Seq(
                true, true, true,
                false, false, false, 
                false, true, true, false, true, true, true, 
                true, true, false
            )
            test === expected
        }
        assert {
            val test = AssignedC(Some(Dest.PURE_MD), Comp.ZERO, None).binary
            val expected = Seq(
                true, true, true,
                false, true, true, 
                false, true, false, true, false, true, false,
                true, true, true
            )
            test === expected
        }
        assert {
            val test = AssignedC(Some(Dest.PURE_M), Comp.ZERO, Some(Jump.JLT)).binary
            val expected = Seq(
                true, true, true,
                false, false, true, 
                false, true, false, true, false, true, false,
                false, true, true
            )
            test === expected
        }
        assert {
            val test = AssignedC(Some(Dest.PURE_A), Comp.M_MINUS_D, Some(Jump.JGT)).binary
            val expected = Seq(
                true, true, true,
                true, false, false, 
                true, false, false, false, true, true, true,
                false, false, false
            )
            test === expected
        }
    }

end AssemblyLineSpec