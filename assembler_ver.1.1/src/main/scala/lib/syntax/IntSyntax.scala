package lib.syntax

import cats.*
import cats.data.*
import cats.implicits.*

object IntSyntax:
  extension (n: Int) def toBooleanOption: Option[Boolean] =
    if n === 1 then Some(true)
    else if n === 0 then Some(false)
    else None

