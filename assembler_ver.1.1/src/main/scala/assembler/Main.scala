package assembler

import cats.*
import cats.data.*
import cats.implicits.*
import cats.effect.*
import cats.kernel.instances.all.*

import assembler.module.ParserModule

object Main extends App, ParserModule:
  val testData = "D=M+1"
  val result = parseAll(instructionCParser, testData)
  println(result)