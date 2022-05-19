package assembler.data

import lib.*
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

  extension (a: AssignedA)
    def binary(using BinaryConvertor[Int]): Seq[Boolean] = 
      val temp = BinaryConvertor[Int].binary(a.number)
      val div  = Seq.fill(16 - temp.length)(false)
      div ++ temp
  
  extension (c: AssignedC)
    def binary: Seq[Boolean] = 
      Seq(true, true, true) ++ c.dest.binary ++ c.comp.binary ++ c.jump.binary

  sealed abstract class PassedInstruction

  sealed case class PassedLabel(line: String) extends PassedInstruction

  object AssignedInstruction:
    def unapply(a: AssignedInstruction): true = true
  
  sealed abstract class AssignedInstruction extends PassedInstruction

  case class PassedA(variable: String) extends PassedInstruction

  case class AssignedA(number: Int) extends AssignedInstruction

  case class AssignedC(dest: Dest, comp: Comp, jump: Jump) extends AssignedInstruction
   