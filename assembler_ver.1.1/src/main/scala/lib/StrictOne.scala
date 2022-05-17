package lib

import cats.*
import cats.data.*
import cats.implicits.*

import simulacrum.typeclass
import scala.annotation.implicitNotFound


object StrictOne:
  @inline def apply[F[_]](using instance: StrictOne[F]): StrictOne[F] = instance

@implicitNotFound("Could not find an instance of StrictOne for ${F}")
@typeclass
trait StrictOne[F[_]]:
  def strict[A](a: F[A])(using Monoid[A], Eq[A]): F[A]

