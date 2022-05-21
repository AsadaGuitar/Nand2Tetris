package lib.syntax

import cats.{Eq, Monoid}
import lib.StrictOne


object OptionSyntax:

  extension [A](a: Option[A])(using StrictOne[Option])
    def strict(using Monoid[A], Eq[A]): Option[A] = StrictOne[Option].strict(a)
  
  given StrictOne[Option] with
    def strict[A](a: Option[A])(using Monoid[A], Eq[A]): Option[A] =
      a.flatMap {
        case b if Monoid[A].isEmpty(b) => None
        case b => Some(b)
      }
    