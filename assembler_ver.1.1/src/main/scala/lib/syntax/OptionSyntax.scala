package lib.syntax

import cats.{Eq, Monoid}
import lib.StrictOne

object OptionSyntax:
  given StrictOne[Option] with
    extension [A](a: Option[A])
      def strict(using Monoid[A], Eq[A]): Option[A] =
        a.flatMap {
          case b if Monoid[A].isEmpty(b) => None
          case b => Some(b)
        }
