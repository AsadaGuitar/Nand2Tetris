import cats.implicits._

object CodeModule {

  val destBinary: String => Option[String] ={
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

  val compBinary: String => Option[String] ={
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

  val jumpBinary: String => Option[String] ={
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
}

trait CodeModule {
  import CodeModule._
  import AssemblyRegex._
  import Utility._

  private val commandBinary: String => Option[String] ={
    case line@DCJ_Pattern() =>
      val Array(dest, temp) = line.split("=").take(2)
      val Array(comp, jump) = temp.split(";").take(2)
      "111".some |+| destBinary(dest) |+| compBinary(comp) |+| jumpBinary(jump)
    case line@DC_Pattern() =>
      val Array(dest, comp) = line.split("=").take(2)
      val jump = "null"
      "111".some |+| destBinary(dest) |+| compBinary(comp) |+| jumpBinary(jump)
    case line@CJ_Pattern() =>
      val Array(comp, jump) = line.split(";").take(2)
      val dest = "null"
      "111".some |+| destBinary(dest) |+| compBinary(comp) |+| jumpBinary(jump) case _ => None
  }

  private val addressBinary: String => Option[String] ={
    _.tail.toIntOption.flatMap{ address =>
      bin16(address).map('0' + _.mkString.tail).toOption
    }
  }

  val assemblyBinary: Seq[String] => Option[Seq[String]] ={
    _.map{
      case line@aCommandPattern() => addressBinary(line)
      case line@mnemonicPattern() => commandBinary(line)
      case _ => None
    }.sequence
  }
}
