package assembler.data

import cats.implicits._

object Binary:
  
  type Binary = Array[Boolean]

  trait BinaryConvertor[A]:
    def binary(a: A): Binary
    def binaryOption(a: A): Option[Binary]
