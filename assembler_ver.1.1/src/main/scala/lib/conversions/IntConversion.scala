package lib.conversions

import cats.*
import cats.data.*
import cats.implicits.*

import lib.syntax.IntSyntax._

object IntConversion:

    given IntToBinaryString: Conversion[Int, String] with
        override def apply(x: Int): String = x.toBinaryString

