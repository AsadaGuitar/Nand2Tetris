import scala.util.matching.Regex

object AssemblyRegex {

  val labelPattern: Regex    = "\\(.+?\\)".r
  val aCommandPattern: Regex = "^@.*?$".r
  val numberPattern: Regex   = "^\\d+?$".r
  val nextLinePattern: Regex = "\n|\r\n|\r".r
  val symbolPattern: Regex   = "^\\D.*$".r
  val mnemonicPattern: Regex = "^(?![a-z]).*$".r
  val commentPattern: Regex  = "^//.*$".r
  val DCJ_Pattern: Regex     = ".*=.*;.*".r
  val DC_Pattern: Regex      = ".*=.*".r
  val CJ_Pattern: Regex      = ".*;.*".r
  val registerPattern: Regex = "^R([0-9]|[1][0-5])".r
}
