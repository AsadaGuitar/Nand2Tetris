package assembler.data

import lib.*
import lib.syntax.IntSyntax.{*, given}
import lib.syntax.StringSyntax.{*, given}

import cats.*
import cats.data.*
import cats.implicits.*

import assembler.data.Binary._
import assembler.data.Mnemonic._

import scala.annotation.tailrec

object AssemblyLine:

  abstract class AssemblyLine

  case class CommandA(address: String) extends AssemblyLine
  case class CommandC(dest: Option[String], comp: String, jump: Option[String]) extends AssemblyLine
  case class Label(symbol: String) extends AssemblyLine

  abstract class PassedAssembly extends AssemblyLine
    def toBinaryOption: Option[Binary16]

  case class InstructionA(line: String) extends AssemblyLine
    def toBinaryOption: Option[Binary16] = line.binary

  case class AssignedAddress(number: Int) extends PassedAssembly:
    def toBinaryOption: Option[Binary16] = number.binary
  case class InstructionC(dest: Option[Dest], comp: Comp, jump: Option[Jump]) extends PassedAssembly:
    def toBinaryOption: Option[Binary16] =
      val bin16: Binary = dest.orElseEmpty.binary ++ comp.binary ++ jump.orElseEmpty.binary
       Some(Binary16(bin16))


  // a: PassedA => AssignedA => Binary16
  // c:            AssignedC => Binary16
  // l: PassedLabel

  sealed abstract class PassedInstruction

  sealed abstract class AssignedInstruction extends PassedInstruction:
    def toBinaryEither: Either[String, Binary16]

  // Binary16
  object Binary16:
    def fromBinary(binary: Binary): Either[String, Binary16] =
      val length = binary.length
      if length > 16 then Left("Out bounds.")
      else 
        val division = 16 - length
        Binary16( Array(true).fill ++ binary )

  private[Binary16] class Binary16(binary: Binary)

  case class PassedA(val line: String) extends PassedInstruction

  sealed case class PassedLabel(line: String) extends PassedInstruction
  
  case class AssignedA(val number: Int) extends InstractionA, AssignedInstruction:
    def toBinaryEither: Either[String, Binary16] = Binary16.fromBinary(number.binary)

  case class AssignedC(dest: Option[Dest], comp: Comp, jump: Option[Jump]) extends AssignedInstruction:
    def toBinaryEither: Either[String, Binary16] = 
      val binary = dest.orElseEmpty.binary ++ comp.binary ++ jump.orElseEmpty.binary
      Binary16.fromBinary(binary)