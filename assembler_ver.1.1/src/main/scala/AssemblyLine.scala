import lib._

object AssemblyLine:
   abstract class AssemblyLine
   abstract class Instruction extends AssemblyLine
   case class CommandA(address: String) extends Instruction
   case class CommandC(dest: Option[String], comp: String, jump: Option[String]) extends Instruction
   case class Label(symbol: String) extends AssemblyLine
   