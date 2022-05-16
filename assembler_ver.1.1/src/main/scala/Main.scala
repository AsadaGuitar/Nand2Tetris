import cats.*
import cats.data.*
import cats.implicits.*
import cats.effect.*
import cats.kernel.instances.all.*

import scala.language.postfixOps
import lib.StrictOne
import lib.syntax.OptionSyntax.{*, given}

import scala.util.parsing.combinator._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Seq as MutSeq


object Main extends App, ParserModule, SymbolTableModule:
  import AssemblyLine._ 
  val start = System.currentTimeMillis()
  val r = "^(\\-|\\!)[1ADM]$".r
  println(r.matches("-A"))
  println(parseAll(commandCParser, "M=D"))
  val end = System.currentTimeMillis()
  println(s"TIME: ${end - start}")

