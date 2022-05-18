package lib

import simulacrum.typeclass
import scala.annotation.implicitNotFound


object Empty:
  @inline final def apply[A](using empty: Empty[A]): Empty[A] = empty
  extension [A](a: Option[A])(using empty: Empty[A])
    def getOrEmpty: A = empty.getOrEmpty(a)

@implicitNotFound("Could not find an instance of Empty for ${A}")
@typeclass
trait Empty[A]:
  def empty: A
  def getOrEmpty(a: Option[A]): A = a.getOrElse(empty)
    