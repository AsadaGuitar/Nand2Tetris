import cats.*
import cats.data.*
import cats.implicits.*
import lib.StrictOne
import lib.syntax.OptionSyntax.{*, given}

import scala.util.parsing.combinator.*
import AssemblyLine._


object ParserModule:
    private val numberPattern = "\\d".r
    private val symbolPattern = "^([a-zA-Z]|_|\\.|:).+".r
    private val destPattern   = "null|M|D|MD|A|AM|AD|AMD|".r
    private val compPattern   =
        """^0|
          |^(\\-|\\!)?(1|A|D|M|)|
          |D((\\+|\\-)(1|A|M)|(\\&|\\|)(A|M))|
          |A((\\+|\\-)(1|D|M)|(\\&|\\|)(D|M))|
          |M((\\+|\\-)(1|A|D)|(\\&|\\|)(A|D))
          |""".r
    private val jumpPattern   = "null|JGT|JEQ|JGE|JLT|JNE|JLE|JMP".r
    def moldAssembly(assembly: Iterable[String]): Iterable[String] =
        assembly.map{ line =>
            line.split("//").headOption.map { line =>
                line.filter(_ =!= ' ')
            }.strict
        }.flatten


trait ParserModule extends JavaTokenParsers:
    import AssemblyLine._
    import ParserModule._
    private def symbolParser = not(numberPattern) ~> rep1("""[a-zA-Z]|_|\.|:""".r | numberPattern).map(_.mkString(""))
    private def numberParser = rep(numberPattern).map(_.mkString(""))
    def parseAssembly(line: String): ParseResult[AssemblyLine] = parseAll(assemblyParser, line)
    def assemblyParser: Parser[AssemblyLine] = commandCParser | commandAParser | labelParser
    def labelParser   : Parser[Label] = "(" ~> symbolParser <~ ")" ^^ { symbol => Label(symbol) }
    def commandAParser: Parser[CommandA] = "@" ~> (numberParser | symbolParser) ^^ { symbol => CommandA(symbol) }
    def commandCParser: Parser[CommandC] = opt(destPattern <~ "=") ~ compPattern ~ opt(";" ~> jumpPattern) ^^ {
        case dest ~ comp ~ jump => CommandC(dest, comp, jump)
    }
