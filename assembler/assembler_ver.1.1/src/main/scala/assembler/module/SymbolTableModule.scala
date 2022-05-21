package assembler.module

import cats._
import cats.data._
import cats.implicits._

import scala.annotation.tailrec

import assembler.data.AssemblyLine._
import assembler.data.Symbol
import assembler.data.Binary

import scala.util.parsing.combinator._


object SymbolTableModule extends JavaTokenParsers:

    type SymbolTable = Map[String,Int]

    private val variableStart = 1024

    private val defaultSymbolTable: SymbolTable = 
        Symbol.values.map(s => (s.toString, s.address)).toMap

    private def registerParser = "R" ~> "([0-9]$)|(1[0-5]$)".r ^^ { _.toInt}

    def findSymbol(line: String, address: Int, symbols: SymbolTable): Either[SymbolTable, Int] =
        val registerAddress = parseAll(registerParser, line)
        if (registerAddress.successful) then Either.right(registerAddress.get)
        else symbols.get(line) match 
            case None => Either.left(symbols.updated(line, address))
            case Some(existAddress) => Either.right(existAddress)


trait SymbolTableModule:
    import SymbolTableModule._
    import lib.syntax.IntSyntax._
    
    def assignAddress(assembly: List[PassedInstruction]): List[AssignedInstruction] =

        def intToBinary(n: Int): Binary = Binary.binaryOption(n.toBinaryString).get

        @tailrec
        def loop(
            assembly: List[PassedInstruction],
            assigned: List[AssignedInstruction] = List.empty[AssignedInstruction],
            symbols : SymbolTable = defaultSymbolTable,
            position: Int = 1,
            variable: Int = variableStart
        ): List[AssignedInstruction] =
            assembly match 
                case Nil => assigned.reverse
                case head::tail => 
                    head match 
                        case PassedA(symbol) => 
                            symbol.toIntOption match
                                case None => 
                                    findSymbol(symbol,variable,symbols) match
                                        case Left(newSymbols) => 
                                            loop(tail, assigned, newSymbols, position+1, variable+1)
                                        case Right(address)   => 
                                            loop(tail, AssignedA(intToBinary(address)) +: assigned, symbols, position+1, variable)
                                case Some(address) => 
                                    loop(tail, AssignedA(intToBinary(address)) +: assigned, symbols, position+1, variable)
                        case PassedLabel(symbol) => 
                            val newSymbol = symbols.updated(symbol,position)
                            loop(tail, assigned, newSymbol, position, variable)
                        case anyAssigned@AssignedInstruction() => loop(tail, anyAssigned +: assigned, symbols, position+1, variable)
        end loop
        loop(assembly)
