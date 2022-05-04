object ParserModule {
  import AssemblyRegex._

  val removeSpace: Seq[String] => Seq[String] = _.map(_.replace(" ", ""))

  val removeComment: Seq[String] => Seq[String] = _.filter(!commentPattern.matches(_)).map(_.split("//").head)

  val removeEmptyLine: Seq[String] => Seq[String] = _.filter(_.nonEmpty)

  val moldAssembly: Seq[String] => Seq[String] = removeSpace compose removeComment compose removeEmptyLine

  val validateAssembly: String => Boolean = {
    case line@labelPattern()    => symbolPattern.matches(line)
    case line@aCommandPattern() => symbolPattern.matches(line) || numberPattern.matches(line)
    case line => mnemonicPattern.matches(line)
  }
}

trait ParserModule {
  import ParserModule._

  def parseAssembly(assembly: Seq[String]): Seq[String] ={

    moldAssembly(assembly).filter(validateAssembly)
  }
}
