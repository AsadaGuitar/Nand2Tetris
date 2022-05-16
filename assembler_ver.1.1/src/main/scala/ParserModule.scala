import cats.*
import cats.data.*
import cats.implicits.*
import lib.StrictOne
import lib.syntax.OptionSyntax.{*, given}

import scala.util.parsing.combinator.*
import AssemblyLine._


object ParserModule:
    private val numberPattern = "^\\d+$".r
    val symbolPattern   = """(?!^[0-9].*$)[^\\(\\)@][a-zA-Z0-9_:\\.]*""".r
    val mnemonicPattern = """(^(.+=)?.+;.*$)|(^.*=.+(;.+)?$)""".r
    val destPattern     = """^null|M|D|MD|A|AM|AD|AMD$""".r
    val jumpPattern     = """^null|JGT|JEQ|JGE|JLT|JNE|JLE|JMP$""".r
    val compPattern     =
        """^0|
          |^[\\-|\\!]?[1ADM]$|
          |^D(([\\+\\-][1AM])|([\\&\\|][AM]))$|
          |^A(([\\+\\-][1DM])|([\\&\\|][DM]))$|
          |^M(([\\+\\-][1AD])|([\\&\\|][AD]))$
          |""".r
    
    def moldAssembly(assembly: Iterable[String]): Iterable[String] =
        assembly.map{ line =>
            line.split("//").headOption.map { line =>
                line.filter(_ =!= ' ')
            }.strict
        }.flatten


trait ParserModule extends JavaTokenParsers:
    import AssemblyLine._
    import ParserModule._
    def parseAssembly(line: String): ParseResult[AssemblyLine] = parseAll(assemblyParser, line)
    def assemblyParser: Parser[AssemblyLine]   = commandCParser | commandAParser | labelParser
    def labelParser   : Parser[Label]    = "(" ~> symbolPattern <~ ")" ^^ { symbol => Label(symbol) }
    def commandAParser: Parser[CommandA] = "@" ~> (symbolPattern | numberPattern) ^^ { symbol => CommandA(symbol) }
    def commandCParser: Parser[CommandC] = opt(destPattern <~ "=") ~ compPattern ~ opt(";" ~> jumpPattern) ^^ {
            case line@dest ~ comp ~ jump => CommandC(dest, comp, jump)
        }

