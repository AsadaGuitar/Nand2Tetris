import cats._, cats.data._, cats.implicits._
import cats._
import cats.data._
import cats.implicits._
import scala.annotation.tailrec

trait CodeModule {

  private val destBinary: String => Option[String] ={
    case "null" => "000".some
    case "M"    => "001".some
    case "D"    => "010".some
    case "MD"   => "011".some
    case "A"    => "100".some
    case "AM"   => "101".some
    case "AD"   => "110".some
    case "AMD"  => "111".some
    case _      => None
  }

  private val compBinary: String => Option[String] ={
    case "0"   => "0101010".some
    case "1"   => "0111111".some
    case "-1"  => "0111010".some
    case "D"   => "0001100".some
    case "A"   => "0110000".some
    case "!D"  => "0001101".some
    case "!A"  => "0110001".some
    case "-D"  => "0001111".some
    case "-A"  => "0110011".some
    case "D+1" => "0011111".some
    case "A+1" => "0110111".some
    case "D-1" => "0001110".some
    case "A-1" => "0110010".some
    case "D+A" => "0000010".some
    case "D-A" => "0010011".some
    case "A-D" => "0000111".some
    case "D&A" => "0000000".some
    case "D|A" => "0010101".some
    case "M"   => "1110000".some
    case "!M"  => "1110001".some
    case "-M"  => "1110011".some
    case "M+1" => "1110111".some
    case "M-1" => "1110010".some
    case "D+M" => "1000010".some
    case "D-M" => "1010011".some
    case "M-D" => "1000111".some
    case "D&M" => "1000000".some
    case "D|M" => "1010101".some
    case _     => None
  }

  private val jumpBinary: String => Option[String] ={
    case "null" => "000".some
    case "JGT"  => "001".some
    case "JEQ"  => "010".some
    case "JGE"  => "011".some
    case "JLT"  => "100".some
    case "JNE"  => "101".some
    case "JLE"  => "110".some
    case "JMP"  => "111".some
    case _      => None
  }

  /**
   * ex)
   *    "D=M" => "1110 0011 0000 1000"
   */
  def commandBinary(line: String): Option[String]  = {

    val cmd = line.filter(_ != ' ') match {
      // D=A;JMP
      case line if line.matches(".*=.*;.*") =>
        val Array(dest, temp) = line.split("=").take(2)
        val Array(comp, jump) = temp.split(";").take(2)
        destBinary(dest) |+| compBinary(comp) |+| jumpBinary(jump)
      // M=1
      case line if line.matches(".*=.*") =>
        val Array(dest, comp) = line.split("=").take(2)
        val jump = "null"
        destBinary(dest) |+| compBinary(comp) |+| jumpBinary(jump)
      // 0;JMP
      case line if line.matches(".*;.*") =>
        val Array(comp, jump) = line.split(";").take(2)
        val dest = "null"
        destBinary(dest) |+| compBinary(comp) |+| jumpBinary(jump)
      case _ => None
    }

    cmd.map("111" ++ _)
  }

  /**
   * ex)
   *    "@16" => "0000000000010000"
   */
  def addressBinary(address: String): Option[String] = {
    address.toIntOption.flatMap(a => bin16(a).map('0' + _.mkString.tail).toOption)
  }

  private def bin16(number: Int): Either[IndexOutOfBoundsException, Array[Int]] = {
    val bin = new Array[Int](16)
    @tailrec
    def loop(n: Int, idx: Int = 15): Either[IndexOutOfBoundsException, Array[Int]] = {
      if (n < 2) {
        bin(idx) = n % 2
        Right(bin)
      } else {
        bin(idx) = n % 2
        if (idx == 0) {
          Left(new IndexOutOfBoundsException("IndexOutOfBoundsException from 'bin16'."))
        }
        loop(n / 2, idx -1)
      }
    }
    loop(number)
  }
}
