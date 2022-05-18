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
import lib.syntax.IntSyntax.{_, given}


object Main extends App, ParserModule:
  val testData = 0
  val rs = BinaryConvertor[Int].binary(testData)
  println(rs)
end Main