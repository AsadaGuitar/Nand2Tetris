import cats.*
import cats.data.*
import cats.implicits.*
import lib.StrictOne
import lib.syntax.OptionSyntax.{*, given}

import scala.util.parsing.combinator.*
import AssemblyLine._


object ParserModule:
    private def moldAssembly(assembly: Iterable[String]) =
        assembly.map{ line =>
            line.split("//").headOption.map { line =>
                line.filter(_ =!= ' ')
            }.strict
        }.flatten


trait ParserModule extends JavaTokenParsers:
    import ParserModule._
    def parseAssembly(assembly: Iterable[String]): Iterable[ParseResult[AssemblyLine]] =
        moldAssembly(assembly).map { (line: String) =>
            parse(assemblyParser, line)
        }
    private def assemblyParser: Parser[AssemblyLine] = labelParser | commandAParser | commandCParser
    private def labelParser = "(" ~ stringLiteral ~ ")" ^^ { case _ ~ label ~ _ => Label(label) }
    private def commandAParser = "@" ~ stringLiteral ^^ { case _ ~ line => CommandA(line) }
    private def commandCParser = opt(stringLiteral ~ "=") ~ stringLiteral ~ opt(";" ~ stringLiteral) ^^ {
        case dest ~ comp ~ jump =>
            CommandC(
                dest.map { case dest ~ _ => dest },
                comp,
                jump.map { case jump ~ _ => jump}
            )
    }
