package assembler

import cats.*
import cats.data.*
import cats.implicits.*
import cats.effect.*
import cats.kernel.instances.all.*

import assembler.module.ParserModule
import assembler.data.Mnemonic.{_, given}

import lib.Empty._


import lib._
import lib.syntax.StringSyntax.{_, given}



object Main extends App, ParserModule:
  val testData = "000"
  val rs = BinaryConvertor[String].binary(testData)
  println(rs)
end Main