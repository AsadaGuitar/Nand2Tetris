import scala.util.matching.Regex

object VmRegex {
  val commentPattern: Regex = "^//.*$".r
}
