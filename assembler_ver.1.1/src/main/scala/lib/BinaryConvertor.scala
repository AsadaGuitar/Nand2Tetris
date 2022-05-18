package lib

import simulacrum.typeclass
import scala.annotation.implicitNotFound


object BinaryConvertor:
  @inline def apply[A](using instance: BinaryConvertor[A]): BinaryConvertor[A] = instance

@implicitNotFound("Could not find an instance of BinnaryConvertor for {A}")
@typeclass
trait BinaryConvertor[A]:
  def binary(a: A): Array[Boolean]
  def binaryOption(a: A): Option[Array[Boolean]]
