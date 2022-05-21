package assembler.data

import lib.*
import lib.syntax.IntSyntax.{_, given}

import cats.*
import cats.data.*
import cats.implicits.*

import assembler.data.Mnemonic._
import assembler.data.Binary.{_, given}
import assembler.data.Symbol._


object AssemblyLine:

  sealed abstract class PassedInstruction

  sealed case class PassedLabel(line: String) extends PassedInstruction

  object AssignedInstruction:
    def unapply(a: AssignedInstruction): true = true
  
  abstract class AssignedInstruction extends PassedInstruction:
    def binary16: Binary

  case class PassedA(variable: String) extends PassedInstruction

  case class AssignedA(binary: Binary) extends AssignedInstruction:
    def binary16: Binary = 
      val binaryString = Seq.fill(16 - binary.toString.length)("0").mkString("") + binary.toString
      Binary(binaryString)

  case class AssignedC(dest: Dest, comp: Comp, jump: Jump) extends AssignedInstruction:
    def binary16: Binary = Binary("111") |+| dest.binary |+| comp.binary |+| jump.binary
