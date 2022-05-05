trait ParserModule {
  import AssemblyRegex._

  private val removeSpace = (_: Seq[String]).map(_.replace(" ", ""))

  private val removeComment = (_: Seq[String]).filter(!commentPattern.matches(_)).map(_.split("//").head)

  private val removeEmptyLine = (_: Seq[String]).filter(_.nonEmpty)

  private val moldAssembly = removeSpace compose removeComment compose removeEmptyLine

  private val validateAssembly: String => Boolean = {
    case line@labelPattern()    => symbolPattern.matches(line)
    case line@aCommandPattern() => symbolPattern.matches(line) || numberPattern.matches(line)
    case line => mnemonicPattern.matches(line)
  }

  val parseAssembly: Seq[String] => Seq[String] = moldAssembly(_).filter(validateAssembly)
}
