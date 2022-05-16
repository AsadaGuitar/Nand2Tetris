import lib._
import lib.syntax.IntSyntax._
import lib.syntax.StringSyntax._

import cats.*
import cats.data.*
import cats.implicits.*


import scala.annotation.tailrec

object AssemblyLine:

   abstract class AssemblyLine

   abstract class Instruction extends AssemblyLine

   case class CommandA(address: String) extends Instruction

   case class CommandC(dest: Option[String], comp: String, jump: Option[String]) extends Instruction

   case class Label(symbol: String) extends AssemblyLine


//   class Binary16(address: Int):
//      val binary: Binary
//      def bin16(number: Int): Either[IndexOutOfBoundsException, Array[Int]] =
//         val bin = new Array[Int](16)
//         @tailrec
//         def loop(n: Int, idx: Int = 15): Either[IndexOutOfBoundsException, Array[Int]] =
//            if n < 2 then
//               bin(idx) = n % 2
//               Right(bin)
//            else
//               bin(idx) = n % 2
//               if idx == 0 then
//                  Left(new IndexOutOfBoundsException("IndexOutOfBoundsException from 'bin16'."))
//               loop(n / 2, idx -1)
//         loop(number)
//      end bin16


   type Binary = Array[Boolean]

   case class Binary16(value: Binary):
      require(value.length === 16)

   trait BinaryConvertor:
      def toBinaryOption: Option[Binary16]

   abstract class _AssemblyLine
   abstract class PassedAssembly extends _AssemblyLine with BinaryConvertor

   case class InstructionA(line: String) extends _AssemblyLine

   case class AssignedAddress(number: Int) extends PassedAssembly:
      def toBinaryOption: Option[Binary16] =
         @tailrec
         def binary(n: Int, bin: List[Int] = List.empty[Int]): List[Int] =
            if n / 2 == 1 then 1 :: (n % 2) :: bin
            else
               val r = n % 2
               val q = n / 2
               binary(q, r :: bin)
         binary(number).map(_.toBooleanOption).sequence.flatMap { (bin: List[Boolean]) =>
           try Some( Binary16(bin.toArray) )
           catch
              case _: IllegalArgumentException => None
         }

   case class InstructionC(dest: Option[Dest], comp: Comp, jump: Option[Jump]) extends PassedAssembly:
      def toBinaryOption: Option[Binary16] = ???

   case class Rabel(symbol: String) extends _AssemblyLine

   abstract class Mnemonic(binary: Binary, operand: String)

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