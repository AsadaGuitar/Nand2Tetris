import lib.syntax.StringSyntax.*

import Binary.*

import cats.*
import cats.data._
import cats.implicits.*

import lib.Empty

import scala.language.postfixOps


object Mnemonic:

  extension [A <: Mnemonic](mnemonic: Option[A])
    def orElseEmpty(using Empty[A]): A = mnemonic.getOrElse(Empty[A].empty)

  given Empty[Dest] with
    override def empty: Dest = Dest.NULL

  given Empty[Comp] with
    override def empty: Comp = Comp.ZERO

  given Empty[Jump] with
    override def empty: Jump = Jump.NULL


  abstract class Mnemonic(val binary: Binary, val operand: String)

  enum Dest(binary: Binary, operand: String) extends Mnemonic(binary, operand):
    case NULL     extends Dest("000".toBinary, "null")
    case PURE_M   extends Dest("001".toBinary, "M")
    case PURE_D   extends Dest("010".toBinary, "D")
    case PURE_MD  extends Dest("011".toBinary, "MD")
    case PURE_A   extends Dest("100".toBinary, "A")
    case PURE_AM  extends Dest("101".toBinary, "AM")
    case PURE_AD  extends Dest("110".toBinary, "AD")
    case PURE_AMD extends Dest("111".toBinary, "AMD")

  enum Comp(binary: Binary, operand: String) extends Mnemonic(binary, operand):
    case ZERO        extends Comp("0101010".toBinary, "0")
    case ONE         extends Comp("0111111".toBinary, "1")
    case MINUS_ONE   extends Comp("0111010".toBinary, "-1")
    case PURE_D      extends Comp("0001100".toBinary, "D")
    case PURE_A      extends Comp("0110000".toBinary, "A")
    case NOT_D       extends Comp("0001101".toBinary, "!D")
    case NOT_A       extends Comp("0110001".toBinary, "!A")
    case MINUS_D     extends Comp("0001111".toBinary, "-D")
    case MINUS_A     extends Comp("0110011".toBinary, "-A")
    case D_PLUS_ONE  extends Comp("0011111".toBinary, "D+1")
    case A_PLUS_ONE  extends Comp("0110111".toBinary, "A+1")
    case D_MINUS_ONE extends Comp("0001110".toBinary, "D-1")
    case A_MINUS_ONE extends Comp("0110010".toBinary, "A-1")
    case D_PLUS_A    extends Comp("0000010".toBinary, "D+A")
    case D_MINUS_A   extends Comp("0010011".toBinary, "D-A")
    case A_MINUS_D   extends Comp("0000111".toBinary, "A-D")
    case D_AND_A     extends Comp("0000000".toBinary, "D&A")
    case D_OR_A      extends Comp("0010101".toBinary, "D|A")
    case PURE_M      extends Comp("1110000".toBinary, "M")
    case NOT_M       extends Comp("1110001".toBinary, "!M")
    case MINUS_M     extends Comp("1110011".toBinary, "-M")
    case M_PLUS_ONE  extends Comp("1110111".toBinary, "M+1")
    case M_MINUS_ONE extends Comp("1110010".toBinary, "M-1")
    case D_PLUS_M    extends Comp("1000010".toBinary, "D+M")
    case D_MINUS_M   extends Comp("1010011".toBinary, "D-M")
    case M_MINUS_D   extends Comp("1000111".toBinary, "M-D")
    case D_AND_M     extends Comp("1000000".toBinary, "D&M")
    case D_OR_M      extends Comp("1010101".toBinary, "D|M")

  enum Jump(binary: Binary, operand: String) extends Mnemonic(binary, operand):
    case JGT  extends Jump("000".toBinary, "JGT")
    case JEQ  extends Jump("001".toBinary, "JEQ")
    case JGE  extends Jump("010".toBinary, "JGE")
    case JLT  extends Jump("011".toBinary, "JLT")
    case JNE  extends Jump("100".toBinary, "JNE")
    case JLE  extends Jump("101".toBinary, "JLE")
    case JMP  extends Jump("110".toBinary, "JMP")
    case NULL extends Jump("111".toBinary, "null")
