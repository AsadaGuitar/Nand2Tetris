package lib.syntax

import cats.*
import cats.data.*
import cats.implicits.*

import scala.annotation.tailrec


object IntSyntax:

  extension (n: Int) 
    def toBinaryString: String =
      @tailrec
      def loop(n: Int, bin: Seq[String] = Seq.empty[String]): String =
        val r = n % 2
        n / 2 match
          case i if i < 2 => 
            (i +: r +: bin).map(_.toString).mkString("")
          case _ =>
            val q = n /2
            loop(q, r.toString +: bin)
      if n > 1 then loop(n)
      else n.toString
