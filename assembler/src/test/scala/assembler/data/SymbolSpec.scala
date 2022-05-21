package assembler.data

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import org.scalatest.PrivateMethodTester

import assembler.data.AssemblyLine.{_, given}
import assembler.data.Mnemonic.{_, given}
import assembler.data.Symbol.{_, given}


class SymbolSpec extends AnyFlatSpec with Diagrams with PrivateMethodTester:

    /**
     *  TestType:
     *    All
     *  Function: 
     *    Symbol#findAddress(line: String): Option[Int]
     *  Description:
     *    Find address value of enum 'Symbol' by symbol's name.
     */
    "Symbol#findAddress" should "can find enum 'Symbol' by name." in {
        assert(Symbol.findAddress("SP") === Some(0))
        assert(Symbol.findAddress("THIS") === Some(3))
        assert(Symbol.findAddress("SCREEN") === Some(16384))
        assert(Symbol.findAddress("R0") === Some(0))
        assert(Symbol.findAddress("R9") === Some(9))
        assert(Symbol.findAddress("R10") === Some(10))
        assert(Symbol.findAddress("R15") === Some(15))
        assert(Symbol.findAddress("R16") === None)
        assert(Symbol.findAddress("TEST") === None)
    }
