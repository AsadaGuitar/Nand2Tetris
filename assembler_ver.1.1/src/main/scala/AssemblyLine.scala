import lib._

object AssemblyLine:
   abstract class AssemblyLine
   case class CommandA(address: String) extends AssemblyLine
   case class CommandC(dest: Option[String], comp: String, jump: Option[String]) extends AssemblyLine
   case class Label(symbol: String) extends AssemblyLine
   