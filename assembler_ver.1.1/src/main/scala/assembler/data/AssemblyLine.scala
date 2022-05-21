package assembler.data

import lib.*
import lib.syntax.IntSyntax.{_, given}

import cats.*
import cats.data.*
import cats.implicits.*

import lib.BinaryConvertor
import assembler.data.Mnemonic._

// import scala.language.implicitConversions


object AssemblyLine:

  sealed abstract class PassedInstruction

  sealed case class PassedLabel(line: String) extends PassedInstruction

  object AssignedInstruction:
    def unapply(a: AssignedInstruction): true = true
  
  abstract class AssignedInstruction extends PassedInstruction:
    def binary: Seq[Boolean]

  case class PassedA(variable: String) extends PassedInstruction

  case class AssignedA(number: Int) extends AssignedInstruction:
    val binary: Seq[Boolean] = 
      val temp = BinaryConvertor[Int].binary(number)
      Seq.fill(16 - temp.length)(false) ++ temp

  case class AssignedC(dest: Dest, comp: Comp, jump: Jump) extends AssignedInstruction:
    val binary: Seq[Boolean] = 
      Seq(true, true, true) ++ dest.binary ++ comp.binary ++ jump.binary
