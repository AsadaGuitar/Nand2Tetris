import lib.*
import lib.syntax.IntSyntax.*
import lib.syntax.StringSyntax.*
import cats.*
import cats.data.*
import cats.implicits.*
import Binary.*

import Mnemonic._

import scala.annotation.tailrec

object AssemblyLine:

  abstract class AssemblyLine

  case class CommandA(address: String) extends AssemblyLine
  case class CommandC(dest: Option[String], comp: String, jump: Option[String]) extends AssemblyLine
  case class Label(symbol: String) extends AssemblyLine


  abstract class PassedAssembly extends AssemblyLine with BinaryConvertor

  case class InstructionA(line: String) extends AssemblyLine

  case class AssignedAddress(number: Int) extends PassedAssembly:
    def toBinaryOption: Option[Binary16] =
      @tailrec
      def binary(n: Int, bin: List[Int] = List.empty[Int]): List[Int] =
        if n / 2 == 1 then 1 :: (n % 2) :: bin
        else
          val q = n / 2; val r = n % 2
          binary(q, r :: bin)
      binary(number).map(_.toBooleanOption).sequence.flatMap { (bin: List[Boolean]) =>
        try Some(Binary16(bin.toArray))
        catch case _: IllegalArgumentException => None
      }

  case class InstructionC(dest: Option[Dest], comp: Comp, jump: Option[Jump]) extends PassedAssembly:
    def toBinaryOption: Option[Binary16] =
      val bin16: Binary = dest.orElseEmpty.binary ++ comp.binary ++ jump.orElseEmpty.binary
       Some(Binary16(bin16))

