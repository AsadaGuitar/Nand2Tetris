package assembler.module

import cats.*
import cats.data.*
import cats.implicits.*

import scala.annotation.tailrec

import assembler.data.AssemblyLine._


case class AssemblySymbol(symbol: String, address: Int)


trait SymbolTableModule:
//  def assignSymbol(assembly: List[AssemblyLine]): List[AssemblyLine] =
//    val numberPattern  = "^\\d+?$".r
//    @tailrec
//    def loop(assembly : List[AssemblyLine],
//             assigned : List[AssemblyLine],
//             symbols  : List[AssemblySymbol],
//             position : Int,
//             variable : Int): List[AssemblyLine] =
//      assembly match
//        case Nil => assigned.reverse
//        case head::tail =>
//          head match
//            case CommandA(line) if !numberPattern.matches(line) =>
//              symbols.find(_.symbol == line) match
//                case None =>
//                  val appendedSymbols = AssemblySymbol(line,variable) +: symbols
//                  loop(tail, assigned, appendedSymbols, position+1, variable+1)
//                case Some(symbol) =>
//                  val appendedAssigned = CommandA(symbol.address.toString) +: assigned
//                  loop(tail, appendedAssigned, symbols, position+1, variable)
//            case Label(symbol) =>
//              val appendedSymbols = AssemblySymbol(symbol,position) +: symbols
//              loop(tail, assigned, appendedSymbols, position+1, variable)
//            case _ =>
//              val appendedAssigned = head +: assigned
//              loop(tail, appendedAssigned, symbols, position+1, variable)
//    end loop
//    loop(assembly, Nil, Nil, 1, 1024)
    