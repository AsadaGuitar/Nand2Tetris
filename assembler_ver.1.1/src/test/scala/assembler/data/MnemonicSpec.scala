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

    /**
     *  TestType:
     *    All
     *  Function: 
     *    Empty[Dest]#empty: Dest
     *  Description:
     *    Return empty value of enum 'Dest'.
     */
    "Empty[Dest]#empty" should "return Dest.NULL" in {
        assert(Empty[Dest].empty === Dest.NULL)
    }

    /**
     *  TestType:
     *    All
     *  Function: 
     *    Empty[Dest]#getOrEmpty: Dest
     *  Description:
     *    Returns the value from Dest when the value does None.
     */
    "Empty[Dest]#getOrEmpty" should "return Dest.Null or some value." in {
        assert(Empty[Dest].getOrEmpty(Dest.PURE_D.some) === Dest.PURE_D)
        assert{
            val test: Option[Dest] = None
            Empty[Dest].getOrEmpty(test) === Dest.NULL
        }
    }

    /**
     *  TestType:
     *    All
     *  Function: 
     *    Empty[Comp]#empty: Comp
     *  Description:
     *    Return empty value of enum 'Comp'.
     */
    "Empty[Comp]#empty" should "return Comp.ZERO" in {
        assert(Empty[Comp].empty === Comp.ZERO)
    }

    /**
     *  TestType:
     *    All
     *  Function: 
     *    Empty[Comp]#getOrEmpty: Comp
     *  Description:
     *    Returns the value from Comp when the value does None.
     */
    "Empty[Comp]#getOrEmpty" should "return Comp.ZERO or some value." in {
        assert(Empty[Comp].getOrEmpty(Comp.D_AND_A.some) === Comp.D_AND_A)
        assert{
            val test: Option[Comp] = None
            Empty[Comp].getOrEmpty(test) === Comp.ZERO
        }
    }

    /**
     *  TestType:
     *    All
     *  Function: 
     *    Empty[Jump]#empty: Jump
     *  Description:
     *    Return empty value of enum 'Jump'.
     */
    "Empty[Jump]#empty" should "return Jump.NULL" in {
        assert(Empty[Jump].empty === Jump.NULL)
    }

    /**
     *  TestType:
     *    All
     *  Function: 
     *    Empty[Jump]#getOrEmpty: Jump
     *  Description:
     *    Returns the value from Jump when the value does None.
     */
    "Empty[Jump]#getOrEmpty" should "return Jump.NULL or some value." in {
        assert(Empty[Jump].getOrEmpty(Jump.JEQ.some) === Jump.JEQ)
        assert{
            val test: Option[Jump] = None
            Empty[Jump].getOrEmpty(test) === Jump.NULL
        }
    }

    /**
     *  TestType:
     *    All
     *  Function: 
     *    Dest#findByOperand(operand: String): Option[Dest]
     *  Description:
     *    Can search for a value using "operand" from enum Dest.
     */
    "Dest#findByOperand" should "can to find Dest by operand." in {
        assert(Dest.findByOperand("null") === Some(Dest.NULL))
        assert(Dest.findByOperand("D") === Some(Dest.PURE_D))
        assert(Dest.findByOperand("AM") === Some(Dest.PURE_AM))
        assert(Dest.findByOperand("AMD") === Some(Dest.PURE_AMD))
        assert(Dest.findByOperand("TEST") === None)
        assert(Dest.findByOperand("DEST") === None)
    }

    /**
     *  TestType:
     *    All
     *  Function: 
     *    Comp#findByOperand(operand: String): Option[Comp]
     *  Description:
     *    Can search for a value using "operand" from enum Comp.
     */
    "Comp#findByOperand" should "can to find Comp by operand." in {
        assert(Comp.findByOperand("0") === Some(Comp.ZERO))
        assert(Comp.findByOperand("-D") === Some(Comp.MINUS_D))
        assert(Comp.findByOperand("D&A") === Some(Comp.D_AND_A))
        assert(Comp.findByOperand("D-M") === Some(Comp.D_MINUS_M))
        assert(Comp.findByOperand("TEST") === None)
        assert(Comp.findByOperand("COMP") === None)
    }

    /**
     *  TestType:
     *    All
     *  Function: 
     *    Jump#findByOperand(operand: String): Option[Jump]
     *  Description:
     *    Can search for a value using "operand" from enum Jump.
     */
    "Jump#findByOperand" should "can to find Jump by operand." in {
        assert(Jump.findByOperand("JGT") === Some(Jump.JGT))
        assert(Jump.findByOperand("JLE") === Some(Jump.JLE))
        assert(Jump.findByOperand("null") === Some(Jump.NULL))
        assert(Jump.findByOperand("JMP") === Some(Jump.JMP))
        assert(Jump.findByOperand("TEST") === None)
        assert(Jump.findByOperand("JUMP") === None)
    }
