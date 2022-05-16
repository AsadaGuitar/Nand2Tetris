import cats.implicits._

object Binary:
  
  type Binary = Array[Boolean]

  sealed case class Binary16(value: Binary):
    require(value.length === 16)

  trait BinaryConvertor:
    def toBinaryOption: Option[Binary16]
