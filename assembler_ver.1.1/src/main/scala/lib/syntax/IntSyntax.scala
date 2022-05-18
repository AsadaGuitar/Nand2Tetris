package lib.syntax

import cats.*
import cats.data.*
import cats.implicits.*
import lib.BinaryConvertor

import scala.annotation.tailrec
import scala.language.implicitConversions


object IntSyntax:
  given Conversion[Int,Boolean] with
    override def apply(x: Int): Boolean =
      if x === 0 then false else true

  given BinaryConvertor[Int] with
    def binary(number: Int): Array[Boolean] =
      @tailrec
      def loop(n: Int, bin: List[Boolean] = List.empty[Boolean]): List[Boolean] =
        n / 2 match
          case i if i < 2 => i +: bin
          case _ =>
            val r = n % 2; val q = n /2
            loop(q, r +: bin)
      loop(number).toArray
    def binaryOption(number: Int): Option[Array[Boolean]] = Some(binary(number))

end IntSyntax