package lib.syntax

import cats.*
import cats.data.*
import cats.implicits.*
import lib.BinaryConvertor

import scala.annotation.tailrec
import scala.language.implicitConversions


object IntSyntax:

  given BinaryConvertor[Int] with
    import lib.conversions.IntConversion.{_, given}
    def binary(number: Int): Seq[Boolean] =
      @tailrec
      def loop(n: Int, bin: Seq[Boolean] = Seq.empty[Boolean]): Seq[Boolean] =
        val r = n % 2
        n / 2 match
          case i if i < 2 => i +: (r +: bin: Seq[Boolean])
          case _ =>
            val q = n /2
            loop(q, r +: bin)
      if number > 1 then loop(number) else Seq(number)

end IntSyntax