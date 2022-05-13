trait Parser {
  import VmRegex._

  private val removeComment = (_: Seq[String]).filter(!commentPattern.matches(_)).map(_.split("//").head)
  private val removeEmptyLine = (_: Seq[String]).filter(_.nonEmpty)
  private val removeSpace = (_: Seq[String]).map(_.replace(" ", ""))
  private val moldVmScript = removeEmptyLine andThen removeSpace andThen removeComment
  private val isVmScriptLine = ???

  val parseVmScript: Seq[String] => Seq[String] = ???

}
