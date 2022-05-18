package assembler.data

import lib.*
import lib.Empty._
import lib.syntax.IntSyntax.{_, given}
import lib.syntax.StringSyntax.{_, given}

import cats.*
import cats.data.*
import cats.implicits.*

import lib.BinaryConvertor
import assembler.data.Mnemonic._

import scala.language.implicitConversions
import scala.annotation.tailrec


object AssemblyLine:

  sealed abstract class PassedInstruction

  sealed case class PassedLabel(line: String) extends PassedInstruction
  
  sealed abstract class AssignedInstruction extends PassedInstruction:
    def binary: Array[Boolean]

  case class PassedA(line: String) extends PassedInstruction

  case class AssignedA(number: Int) extends AssignedInstruction:
    def binary: Array[Boolean] = BinaryConvertor[Int].binary(number)

  case class AssignedC(dest: Option[Dest], comp: Comp, jump: Option[Jump]) extends AssignedInstruction:
    def binary: Array[Boolean] = dest.getOrEmpty.binary ++ comp.binary ++ jump.getOrEmpty.binary
