package assembler.module

import cats.*
import cats.data.*
import cats.implicits.*

import lib.StrictOne
import lib.syntax.OptionSyntax.{*, given}

import scala.util.parsing.combinator.*


object ParserModule:
    private val numberPattern   = "^\\d+$".r
    private val mnemonicPattern = """(^(.+=)?.+;.*$)|(^.*=.+(;.+)?$)""".r
    private val symbolPattern   = """(?!^[0-9].*$)[^\\()@][a-zA-Z0-9_:\\.]*""".r
    private val destPattern     = """^(AMD|MD|AM|AD|null|M|D|A)""".r
    private val compD           = "^D(([+\\-][1AM])|([&|][AM]))".r
    private val compA           = "^A(([+\\-][1DM])|([&|][DM]))".r
    private val compM           = "^M(([+\\-][1AD])|([&|][AD]))".r
    private val compSglOp       = "^[\\-!][1ADM]".r
    private val compSgl         = "^[AMD0]".r
    private val jumpPattern     = """^null|JGT|JEQ|JGE|JLT|JNE|JLE|JMP$""".r

trait ParserModule extends JavaTokenParsers:
    import ParserModule._
    import assembler.data.AssemblyLine._
    import assembler.data.Mnemonic.{_, given}
    def moldAssembly(assembly: Iterable[String]): Iterable[String] =
        assembly.map{ line =>
            line.split("//").headOption.map { line =>
                line.replace(" ", Monoid[String].empty)
            }.strict
        }.flatten
    private def compParser = compD | compA | compM | compSglOp | compSgl
    def labelParser: Parser[PassedLabel] = "(" ~> symbolPattern <~ ")" ^^ { symbol => PassedLabel(symbol) }
    def instructionAParser: Parser[PassedA] = "@" ~> (symbolPattern | numberPattern) ^^ { symbol => PassedA(symbol) }
    def instructionCParser: Parser[AssignedC] = guard(mnemonicPattern) ~>
        opt(destPattern <~ "=") ~ compParser ~ opt(";" ~> jumpPattern) ^^ { 
            case dest ~ comp ~ jump => 
                AssignedC(
                    dest.flatMap(operand => FindByOperand[Dest].find(operand)),
                    FindByOperand[Comp].find(comp).get,
                    jump.flatMap(operand => FindByOperand[Jump].find(operand))   
                )
        }
