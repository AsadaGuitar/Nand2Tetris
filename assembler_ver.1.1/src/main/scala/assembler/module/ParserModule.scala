package assembler.module

import cats.*
import cats.data.*
import cats.implicits.*

import lib.StrictOne
import lib.syntax.OptionSyntax.{*, given}

import assembler.data.AssemblyLine._
import assembler.data.Mnemonic.{_, given}
import assembler.data.Symbol

import scala.util.parsing.combinator.*


object ParserModule:
    private val numberPattern   = "^\\d+$".r
    private val mnemonicPattern = """(^(.+=)?.+;.*$)|(^.*=.+(;.+)?$)""".r
    private val symbolPattern   = """(?!^[0-9].*$)[^\\()@][a-zA-Z0-9_:\\.]*""".r
    private val destPattern     = """^(AMD|MD|AM|AD|null|M|D|A)""".r
    private val compD           = "^D(([+\\-][1AM])|([&|][AM]))".r
    private val compA           = "^(A\\-1)|(A\\+[D1])".r
    private val compM           = "^M(([+\\-][1])|(\\-D)|([&|][D]))".r
    private val compSglOp       = "^[\\-!][1ADM]".r
    private val compSgl         = "^[AMD0]".r
    private val jumpPattern     = """^null|JGT|JEQ|JGE|JLT|JNE|JLE|JMP$""".r


trait ParserModule extends JavaTokenParsers:
    import ParserModule._

    def moldAssembly(assembly: Iterable[String]): Iterable[String] =
        assembly.map{ line =>
            line.split("//").headOption.map(_.replace(" ", "")).strict
        }.flatten
    
    private def compParser = compA | compD | compM | compSglOp | compSgl
    
    def labelParser: Parser[PassedLabel] = "(" ~> symbolPattern <~ ")" ^^ { PassedLabel.apply }
    
    def instructionAParser: Parser[PassedA] = "@" ~> (symbolPattern | numberPattern) ^^ { symbol => 
        Symbol.findAddress(symbol) match 
            case None => PassedA(symbol)
            case Some(address) => PassedA(address)
    }
    
    def instructionCParser: Parser[AssignedC] = guard(mnemonicPattern) ~>
        opt(destPattern <~ "=") ~ compParser ~ opt(";" ~> jumpPattern) ^^ { 
            case dest ~ comp ~ jump => 
                AssignedC(
                    dest.flatMap(Dest.findByOperand),
                    Comp.findByOperand(comp).get,
                    jump.flatMap(Jump.findByOperand)   
                )
        }
