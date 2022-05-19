package assembler.module

import cats.*
import cats.data.*
import cats.implicits.*

import scala.annotation.tailrec

import assembler.data.AssemblyLine._
import assembler.data.Symbol


object SymbolTableModule:
    case class AssemblySymbol(symbol: String, address: Int)
    val variableStart = 1024


trait SymbolTableModule:
    import SymbolTableModule._
    
    def assignAddress(assembly: List[PassedInstruction]): List[AssignedInstruction] =
        @tailrec
        def loop(
            assembly: List[PassedInstruction],
            assigned: List[AssignedInstruction] = List.empty[AssignedInstruction],
            symbols : List[AssemblySymbol] = List.empty[AssemblySymbol],
            position: Int = 1,
            variable: Int = variableStart
        ): List[AssignedInstruction] =
            assembly match 
                case Nil => assigned.reverse
                case head::tail => 
                    head match 
                        case PassedA(symbol) => 
                            symbol.toIntOption.orElse(Symbol.findAddress(symbol)) match
                                case None => 
                                    symbols.find(_.symbol === symbol) match 
                                        case None => 
                                            val newSymbol = AssemblySymbol(symbol, variable)
                                            loop(tail, AssignedA(variable) +: assigned, newSymbol +: symbols, position+1, variable+1)
                                        case Some(assignAddress) => 
                                            loop(tail, AssignedA(assignAddress.address) +: assigned, symbols, position+1, variable)
                                case Some(address) => 
                                    loop(tail, AssignedA(address) +: assigned, symbols, position+1, variable)
                        case PassedLabel(symbol) => 
                            val newSymbol = AssemblySymbol(symbol, position)
                            loop(tail, assigned, newSymbol +: symbols, position, variable)
                        case anyAssigned@AssignedInstruction() => loop(tail, anyAssigned +: assigned, symbols, position+1, variable)
        end loop
        loop(assembly)
