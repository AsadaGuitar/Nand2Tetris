import cats.*
import cats.data.*
import cats.implicits.*
import cats.effect.*
import cats.kernel.instances.all.*

import scala.language.postfixOps
import lib.StrictOne
import lib.syntax.OptionSyntax.{*, given}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Seq as MutSeq


object Main extends App, ParserModule, SymbolTableModule:
  val assembly = Vector (
    "@COUNT",
    "M=1",
    "",
    "// comment",
    "@100",
    "D = M",
    "@COUNT // count of function",
    "D = M + D",
    "(Symbol)",
    "@R1",
    "M=D",
    "@Symbol",
    "D = M"
  )
  val start = System.currentTimeMillis()
  val parsed = this.parseAssembly(assembly)
  val end = System.currentTimeMillis()
  println(s"TIME: ${end - start}")


