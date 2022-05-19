package assembler

import cats.*
import cats.data.*
import cats.implicits.*
import cats.effect.*
import cats.kernel.instances.all.*

import assembler.module.ParserModule
import assembler.module.SymbolTableModule

import assembler.data.AssemblyLine.{_, given}
import assembler.data.Symbol.{_, given}
import assembler.data.Mnemonic.{_, given}

import lib.Empty._

import lib._
import lib.syntax.StringSyntax.{_, given}
import lib.syntax.IntSyntax.{_, given}


object Main extends App, ParserModule, SymbolTableModule:
  val r = findAddress("R1")
  println(r)