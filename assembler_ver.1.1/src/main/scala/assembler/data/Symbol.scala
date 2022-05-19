package assembler.data

import cats._
import cats.data._
import cats.implicits._

import scala.util.parsing.combinator._


object Symbol extends JavaTokenParsers:
    
    private def registerParser = "R" ~> "([0-9]$)|(1[0-5]$)".r ^^ { _.toInt}

    def findAddress(line: String): Option[Int] =
        lazy val registerAddress = parseAll(registerParser, line)
        lazy val symbolAddress = Symbol.values.find(_.toString === line)
        if (registerAddress.successful) then Some(registerAddress.get)
        else symbolAddress.map(_.address)


enum Symbol(val address: Int):
    case SP     extends Symbol(0)
    case LCL    extends Symbol(1)
    case ARG    extends Symbol(2)
    case THIS   extends Symbol(3)
    case THAT   extends Symbol(4)
    case SCREEN extends Symbol(16384)
    case KBD    extends Symbol(24576)
    