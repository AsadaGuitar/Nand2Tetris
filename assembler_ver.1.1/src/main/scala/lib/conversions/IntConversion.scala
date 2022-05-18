package lib.conversions

import cats.*
import cats.data.*
import cats.implicits.*

import lib.syntax.{_, given}

object IntConversion:

    given IntToBoolean: Conversion[Int,Boolean] with
        override def apply(x: Int): Boolean =
            if x === 0 then false else true

end IntConversion