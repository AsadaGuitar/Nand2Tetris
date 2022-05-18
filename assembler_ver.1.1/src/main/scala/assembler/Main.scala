package assembler

import cats.*
import cats.data.*
import cats.implicits.*
import cats.effect.*
import cats.kernel.instances.all.*

import assembler.module.ParserModule
import assembler.data.Mnemonic.{_, given}

import lib.Empty._


object Main extends App, ParserModule:
  val testData = "@Symbol"
  val rs = parseAll(instructionAParser, testData)