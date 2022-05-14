package lib

import cats.*
import cats.data.*
import cats.implicits.*
import simulacrum.typeclass

import scala.annotation.implicitNotFound


@implicitNotFound("Could not find an instance of StrictOne for ${F}")
@typeclass
trait StrictOne[F[_]]:
  extension [A](a: F[A])
    def strict(using Monoid[A], Eq[A]): F[A]
