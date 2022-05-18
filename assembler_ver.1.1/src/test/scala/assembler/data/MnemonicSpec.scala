package assembler.data

import cats._
import cats.data._
import cats.implicits._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import lib._
import assembler.data.Mnemonic.{_, given}

class IntSyntaxSpec extends AnyFlatSpec with Diagrams  with PrivateMethodTester:

    "Empty[Dest]#empty" should "return Dest.NULL" in {
        assert(Empty[Dest].empty === Dest.NULL)
    }
    "Empty[Dest]#getOrEmpty" should "return Dest.Null or some value." in {
        assert(Empty[Dest].getOrEmpty(Dest.PURE_D.some) === Dest.PURE_D)
        assert{
            val test: Option[Dest] = None
            Empty[Dest].getOrEmpty(test) === Dest.NULL
        }
    }

    "Empty[Comp]#empty" should "return Comp.ZERO" in {
        assert(Empty[Comp].empty === Comp.ZERO)
    }
    "Empty[Comp]#getOrEmpty" should "return Comp.ZERO or some value." in {
        assert(Empty[Comp].getOrEmpty(Comp.D_AND_A.some) === Comp.D_AND_A)
        assert{
            val test: Option[Comp] = None
            Empty[Comp].getOrEmpty(test) === Comp.ZERO
        }
    }

    "Empty[Jump]#empty" should "return Jump.NULL" in {
        assert(Empty[Jump].empty === Jump.NULL)
    }
    "Empty[Jump]#getOrEmpty" should "return Jump.NULL or some value." in {
        assert(Empty[Jump].getOrEmpty(Jump.JEQ.some) === Jump.JEQ)
        assert{
            val test: Option[Jump] = None
            Empty[Jump].getOrEmpty(test) === Jump.NULL
        }
    }

    "Dest#findByOperand" should "can to find Dest by operand." in {
        assert(Dest.findByOperand("null") === Some(Dest.NULL))
        assert(Dest.findByOperand("D") === Some(Dest.PURE_D))
        assert(Dest.findByOperand("AM") === Some(Dest.PURE_AM))
        assert(Dest.findByOperand("AMD") === Some(Dest.PURE_AMD))
        assert(Dest.findByOperand("TEST") === None)
        assert(Dest.findByOperand("DEST") === None)
    }

    "Comp#findByOperand" should "can to find Comp by operand." in {
        assert(Comp.findByOperand("0") === Some(Comp.ZERO))
        assert(Comp.findByOperand("-D") === Some(Comp.MINUS_D))
        assert(Comp.findByOperand("D&A") === Some(Comp.D_AND_A))
        assert(Comp.findByOperand("D-M") === Some(Comp.D_MINUS_M))
        assert(Comp.findByOperand("TEST") === None)
        assert(Comp.findByOperand("COMP") === None)
    }

    "Jump#findByOperand" should "can to find Jump by operand." in {
        assert(Jump.findByOperand("JGT") === Some(Jump.JGT))
        assert(Jump.findByOperand("JLE") === Some(Jump.JLE))
        assert(Jump.findByOperand("null") === Some(Jump.NULL))
        assert(Jump.findByOperand("JMP") === Some(Jump.JMP))
        assert(Jump.findByOperand("TEST") === None)
        assert(Jump.findByOperand("JUMP") === None)
    }

end IntSyntaxSpec