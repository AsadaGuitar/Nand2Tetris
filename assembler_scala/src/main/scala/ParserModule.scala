
object ParserModule {
  import AssemblyRegex._

  val removeSpace: Seq[String] => Seq[String] = (lines: Seq[String]) =>
    lines.map(_.replace(" ", ""))

  val removeComment: Seq[String] => Seq[String] = (lines: Seq[String]) =>
    lines.filter(!commentPattern.matches(_))

  val moldAssembly: Seq[String] => Seq[String] = removeSpace compose removeComment

  val validateAssembly: String => Boolean = {
    case labelPattern(line)    => symbolPattern.matches(line)
    case aCommandPattern(line) => symbolPattern.matches(line) || numberPattern.matches(line)
    case line => mnemonicPattern.matches(line)
  }
}


trait ParserModule {
  import ParserModule._

  def parseAssembly(assembly: Seq[String]): Seq[String] ={
    moldAssembly(assembly).filter(validateAssembly)
  }
}
