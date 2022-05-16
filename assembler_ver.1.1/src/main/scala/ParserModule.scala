import cats.*
import cats.data.*
import cats.implicits.*
import lib.StrictOne
import lib.syntax.OptionSyntax.{*, given}

import scala.util.parsing.combinator.*
import AssemblyLine._


object ParserModule:
    private val numberPattern   = "^\\d+$".r
    private val mnemonicPattern = """(^(.+=)?.+;.*$)|(^.*=.+(;.+)?$)""".r
    private val symbolPattern   = """(?!^[0-9].*$)[^\\(\\)@][a-zA-Z0-9_:\\.]*""".r
    private val destPattern     = """^(AMD|MD|AM|AD|null|M|D|A)""".r
    private val compD           = "^D(([\\+\\-][1AM])|([\\&\\|][AM]))".r
    private val compA           = "^A(([\\+\\-][1DM])|([\\&\\|][DM]))".r
    private val compM           = "^M(([\\+\\-][1AD])|([\\&\\|][AD]))".r
    private val compSglOp       = "^[\\-\\!][1ADM]".r
    private val compSgl         = "^[AMD0]".r
    private val jumpPattern     = """^null|JGT|JEQ|JGE|JLT|JNE|JLE|JMP$""".r
    
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
    private def compParser = compD | compA | compM | compSglOp | compSgl
    def commandCParser: Parser[CommandC] = guard(mnemonicPattern) ~> 
      opt(destPattern <~ "=") ~ compParser ~ opt(";" ~> jumpPattern) ^^ { 
        case dest ~ comp ~ jump => CommandC(dest, comp, jump)
    }
    
