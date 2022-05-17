package assembler.data

import assembler.data.Binary._

import cats.*
import cats.data._
import cats.implicits.*

import lib.Empty
import lib.syntax.StringSyntax.{*, given}

import scala.language.implicitConversions
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
    case NULL     extends Dest("000", "null")
    case PURE_M   extends Dest("001", "M")
    case PURE_D   extends Dest("010", "D")
    case PURE_MD  extends Dest("011", "MD")
    case PURE_A   extends Dest("100", "A")
    case PURE_AM  extends Dest("101", "AM")
    case PURE_AD  extends Dest("110", "AD")
    case PURE_AMD extends Dest("111", "AMD")

  enum Comp(binary: Binary, operand: String) extends Mnemonic(binary, operand):
    case ZERO        extends Comp("0101010", "0")
    case ONE         extends Comp("0111111", "1")
    case MINUS_ONE   extends Comp("0111010", "-1")
    case PURE_D      extends Comp("0001100", "D")
    case PURE_A      extends Comp("0110000", "A")
    case NOT_D       extends Comp("0001101", "!D")
    case NOT_A       extends Comp("0110001", "!A")
    case MINUS_D     extends Comp("0001111", "-D")
    case MINUS_A     extends Comp("0110011", "-A")
    case D_PLUS_ONE  extends Comp("0011111", "D+1")
    case A_PLUS_ONE  extends Comp("0110111", "A+1")
    case D_MINUS_ONE extends Comp("0001110", "D-1")
    case A_MINUS_ONE extends Comp("0110010", "A-1")
    case D_PLUS_A    extends Comp("0000010", "D+A")
    case D_MINUS_A   extends Comp("0010011", "D-A")
    case A_MINUS_D   extends Comp("0000111", "A-D")
    case D_AND_A     extends Comp("0000000", "D&A")
    case D_OR_A      extends Comp("0010101", "D|A")
    case PURE_M      extends Comp("1110000", "M")
    case NOT_M       extends Comp("1110001", "!M")
    case MINUS_M     extends Comp("1110011", "-M")
    case M_PLUS_ONE  extends Comp("1110111", "M+1")
    case M_MINUS_ONE extends Comp("1110010", "M-1")
    case D_PLUS_M    extends Comp("1000010", "D+M")
    case D_MINUS_M   extends Comp("1010011", "D-M")
    case M_MINUS_D   extends Comp("1000111", "M-D")
    case D_AND_M     extends Comp("1000000", "D&M")
    case D_OR_M      extends Comp("1010101", "D|M")

  enum Jump(binary: Binary, operand: String) extends Mnemonic(binary, operand):
    case JGT  extends Jump("000", "JGT")
    case JEQ  extends Jump("001", "JEQ")
    case JGE  extends Jump("010", "JGE")
    case JLT  extends Jump("011", "JLT")
    case JNE  extends Jump("100", "JNE")
    case JLE  extends Jump("101", "JLE")
    case JMP  extends Jump("110", "JMP")
    case NULL extends Jump("111", "null")
