import scala.util.matching.Regex

object AssemblyRegex {
  val labelPattern: Regex = "\\(.+?\\)".r
  val aCommandPattern: Regex = "^\\@.*".r
}
