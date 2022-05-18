package lib

import simulacrum.typeclass
import scala.annotation.implicitNotFound


object Empty:
  @inline final def apply[A](using empty: Empty[A]): Empty[A] = empty
  

@implicitNotFound("Could not find an instance of Empty for ${A}")
@typeclass
trait Empty[A]:
  def empty: A
  extension (a: Option[A])
    def getOrEmpty: A = a.getOrElse(empty)
    